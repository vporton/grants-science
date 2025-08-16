import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Float "mo:base/Float";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import Order "mo:base/Order";
import Nat64 "mo:base/Nat64";
import Blob "mo:base/Blob";


persistent actor GrantsSystem {
    // Wallet interface types
    public type WalletSubAccount = Blob;
    public type WalletAccountIdentifier = Blob;
    public type WalletTokens = {
        e8s : Nat64;
    };
    public type WalletInfo = {
        principal : Principal;
        subaccount : WalletSubAccount;
        balance : WalletTokens;
        accountId : WalletAccountIdentifier;
    };
    public type WalletTransferRequest = {
        to : WalletAccountIdentifier;
        amount : WalletTokens;
        memo : ?Nat64;
    };
    // Types
    public type TokenType = {
        #ICP;
        #IS20: Text; // Token canister ID
    };
    
    public type DonationSpec = {
        projectId: Text;
        amount: Nat;
        token: TokenType;
        dependencyPercentage: Nat; // X% for dependencies
        affiliatePercentage: Nat; // Y% for affiliate
        affiliate: ?Principal;
        timestamp: Time.Time;
    };
    
    public type ServerInfo = {
        address: Principal;
        pledge: Nat;
        participatesInRewards: Bool;
        errors: Nat;
    };
    
    public type RoundConfig = {
        startTime: Time.Time;
        endTime: Time.Time;
        challengeEndTime: Time.Time;
        worldScienceDAOTax: Nat; // Percentage
        serverRewardPercentage: Nat; // Percentage
        minDonationAmount: Nat;
        affiliateKPercentage: Nat; // K% for previous affiliates
        serverRewardZ: Nat; // Fixed reward per dependency write
    };
    
    public type ProjectStats = {
        totalDonations: Nat;
        donorCount: Nat;
        matchingAmount: Nat;
        affiliates: [(Principal, Nat)]; // Affiliate and their contribution
    };
    
    public type GitCoinPassport = {
        address: Text;
        score: Float;
        timestamp: Time.Time;
    };
    
    public type Project = {
        id: Text;
        githubUrl: Text;
        name: Text;
        description: Text;
        category: { #science; #software };
        owner: Text;
        language: Text;
        stars: Nat;
        forks: Nat;
        topics: [Text];
        createdAt: Text;
        updatedAt: Text;
        submittedAt: Time.Time;
        submittedBy: Principal;
    };
    
    // HTTP types for making requests
    public type HttpRequest = {
        url: Text;
        method: Text;
        body: Text;
        headers: [(Text, Text)];
        transform: ?HttpTransform;
    };
    
    public type HttpResponse = {
        status: Nat;
        headers: [(Text, Text)];
        body: [Nat8];
    };
    
    public type HttpTransform = {
        function: [Nat8];
        context: [Nat8];
    };
    
    // State
    private stable var currentRound: ?RoundConfig = null;
    private transient var donations = HashMap.HashMap<Text, [DonationSpec]>(100, Text.equal, Text.hash);
    private transient var matchingPool = HashMap.HashMap<TokenType, Nat>(10, func(a, b) = a == b, func(t) = 0);
    private transient var servers = HashMap.HashMap<Principal, ServerInfo>(50, Principal.equal, Principal.hash);
    private transient var projectStats = HashMap.HashMap<Text, ProjectStats>(100, Text.equal, Text.hash);
    private transient var projects = HashMap.HashMap<Text, Project>(100, Text.equal, Text.hash);
    private transient var passportScores = HashMap.HashMap<Text, [GitCoinPassport]>(1000, Text.equal, Text.hash);
    private transient var withdrawals = HashMap.HashMap<Text, [(TokenType, Nat)]>(100, Text.equal, Text.hash);
    
    // Wallet canister actor
    private transient let walletCanisterId = "wallet-canister-id"; // This will be set during deployment
    private transient let wallet : actor {
        createWallet : () -> async Result.Result<WalletInfo, Text>;
        getWallet : () -> async Result.Result<WalletInfo, Text>;
        getBalance : () -> async Result.Result<WalletTokens, Text>;
        transfer : (WalletTransferRequest) -> async Result.Result<Nat64, Text>;
        getAccountId : () -> async Result.Result<WalletAccountIdentifier, Text>;
    } = actor(walletCanisterId);
    
    // Start a new funding round
    public shared(msg) func startRound(config: RoundConfig) : async Result.Result<Text, Text> {
        switch (currentRound) {
            case (?_) { #err("A round is already active") };
            case null {
                currentRound := ?config;
                #ok("Round started successfully")
            };
        }
    };
    
    // Add to matching pool
    public shared(msg) func contributeToMatchingPool(
        token: TokenType,
        amount: Nat
    ) : async Result.Result<Text, Text> {
        switch (currentRound) {
            case null { #err("No active round") };
            case (?round) {
                if (Time.now() > round.startTime) {
                    return #err("Cannot contribute to matching pool after round starts");
                };
                
                let current = switch (matchingPool.get(token)) {
                    case null { 0 };
                    case (?amt) { amt };
                };
                
                matchingPool.put(token, current + amount);
                #ok("Added to matching pool")
            };
        }
    };
    
    // Server pledge
    public shared(msg) func serverPledge(
        amount: Nat,
        participateInRewards: Bool
    ) : async Result.Result<Text, Text> {
        switch (currentRound) {
            case null { #err("No active round") };
            case (?round) {
                if (Time.now() > round.startTime) {
                    return #err("Cannot pledge after round starts");
                };
                
                let serverInfo: ServerInfo = {
                    address = msg.caller;
                    pledge = amount;
                    participatesInRewards = participateInRewards;
                    errors = 0;
                };
                
                servers.put(msg.caller, serverInfo);
                
                // Add to matching pool
                let current = switch (matchingPool.get(#ICP)) {
                    case null { 0 };
                    case (?amt) { amt };
                };
                matchingPool.put(#ICP, current + amount);
                
                #ok("Server pledged successfully")
            };
        }
    };
    
    // Make a donation using wallet
    public shared(msg) func donate(spec: DonationSpec) : async Result.Result<Text, Text> {
        switch (currentRound) {
            case null { #err("No active round") };
            case (?round) {
                let now = Time.now();
                if (now < round.startTime or now > round.endTime) {
                    return #err("Not in donation period");
                };
                
                if (spec.amount < round.minDonationAmount) {
                    return #err("Donation below minimum amount");
                };
                
                // Ensure user has a wallet
                let walletResult = await wallet.getWallet();
                switch (walletResult) {
                    case (#err(_)) {
                        // Create wallet if it doesn't exist
                        let createResult = await wallet.createWallet();
                        switch (createResult) {
                            case (#err(e)) { return #err("Failed to create wallet: " # e) };
                            case (#ok(_)) { };
                        };
                    };
                    case (#ok(_)) { };
                };
                
                // Check wallet balance
                let balanceResult = await wallet.getBalance();
                switch (balanceResult) {
                    case (#err(e)) { return #err("Failed to get wallet balance: " # e) };
                    case (#ok(balance)) {
                        if (balance.e8s < Nat64.fromNat(spec.amount)) {
                            return #err("Insufficient wallet balance");
                        };
                    };
                };
                
                // Transfer funds from wallet to grants system (simplified for now)
                // In a real implementation, this would transfer to the grants system's account
                let transferRequest : WalletTransferRequest = {
                    to = Blob.fromArray(Array.freeze(Array.init<Nat8>(32, 0))); // Grants system account
                    amount = { e8s = Nat64.fromNat(spec.amount) };
                    memo = ?Nat64.fromNat(Int.abs(Time.now()));
                };
                
                let transferResult = await wallet.transfer(transferRequest);
                switch (transferResult) {
                    case (#err(e)) { return #err("Transfer failed: " # e) };
                    case (#ok(_)) { };
                };
                
                // Record donation
                let projectDonations = switch (donations.get(spec.projectId)) {
                    case null { [] };
                    case (?existing) { existing };
                };
                
                let newDonations = Array.append(projectDonations, [spec]);
                donations.put(spec.projectId, newDonations);
                
                // Update project stats
                updateProjectStats(spec);
                
                #ok("Donation recorded")
            };
        }
    };
    
    // Submit GitCoin passport score
    public shared(msg) func submitPassportScore(
        address: Text,
        score: Float
    ) : async Result.Result<Text, Text> {
        let passport: GitCoinPassport = {
            address = address;
            score = score;
            timestamp = Time.now();
        };
        
        let scores = switch (passportScores.get(address)) {
            case null { [] };
            case (?existing) { existing };
        };
        
        passportScores.put(address, Array.append(scores, [passport]));
        #ok("Passport score submitted")
    };
    
    // Calculate quadratic matching
    public func calculateMatching(projectId: Text) : async Nat {
        switch (currentRound) {
            case null { 0 };
            case (?round) {
                let projectDonations = switch (donations.get(projectId)) {
                    case null { return 0 };
                    case (?d) { d };
                };
                
                // Group donations by donor and calculate square roots
                let donorTotals = HashMap.HashMap<Principal, Float>(100, Principal.equal, Principal.hash);
                
                for (donation in projectDonations.vals()) {
                    let donor = Principal.fromText(donation.projectId); // This should be donor address
                    let current = switch (donorTotals.get(donor)) {
                        case null { 0.0 };
                        case (?amt) { amt };
                    };
                    
                    // Get passport score
                    let score = getMedianPassportScore(Principal.toText(donor));
                    donorTotals.put(donor, current + Float.fromInt(donation.amount) * score);
                };
                
                // Calculate sum of square roots
                var sumOfSqrts = 0.0;
                for ((_, amount) in donorTotals.entries()) {
                    sumOfSqrts += Float.sqrt(amount);
                };
                
                // Square the sum
                let matchingAmount = sumOfSqrts ** 2;
                
                // Get total matching pool for token type
                let totalPool = switch (matchingPool.get(#ICP)) {
                    case null { 0.0 };
                    case (?amt) { Float.fromInt(amt) };
                };
                
                // Calculate this project's share
                let allProjectsSumSqrts = calculateAllProjectsSumOfSquares();
                let projectShare = if (allProjectsSumSqrts > 0) {
                    matchingAmount / allProjectsSumSqrts
                } else { 0.0 };
                
                Int.abs(Float.toInt(projectShare * totalPool))
            };
        }
    };
    
    // Calculate distribution after round ends
    public shared func calculateDistributions() : async Result.Result<Text, Text> {
        switch (currentRound) {
            case (?round) {
                if (Time.now() < round.endTime) {
                    return #err("Round has not ended yet");
                };
                
                // Calculate distributions for each project
                label l for ((projectId, _) in donations.entries()) {
                    let matching = await calculateMatching(projectId);
                    let stats = switch (projectStats.get(projectId)) {
                        case (?s) { s };
                        case null { continue l };
                    };
                    
                    // Calculate after tax and affiliate fees
                    let totalAmount = stats.totalDonations + matching;
                    let afterTax = totalAmount * (100 - round.worldScienceDAOTax) / 100;
                    
                    // Store withdrawal allowance
                    let current = switch (withdrawals.get(projectId)) {
                        case (?w) { w };
                        case null { [] };
                    };
                    
                    withdrawals.put(projectId, Array.append(current, [(#ICP, afterTax)]));
                };
                
                #ok("Distributions calculated")
            };
            case null { #err("No active round") };
        }
    };
    
    // Withdraw funds
    public shared(msg) func withdraw(projectId: Text) : async Result.Result<Nat, Text> {
        switch (withdrawals.get(projectId)) {
            case null { #err("No funds to withdraw") };
            case (?funds) {
                var total = 0;
                for ((_, amount) in funds.vals()) {
                    total += amount;
                };
                
                // Clear withdrawals
                withdrawals.delete(projectId);
                
                // In real implementation, transfer funds here
                #ok(total)
            };
        }
    };
    
    // Helper functions
    private func updateProjectStats(donation: DonationSpec) {
        let stats = switch (projectStats.get(donation.projectId)) {
            case null {
                {
                    totalDonations = 0;
                    donorCount = 0;
                    matchingAmount = 0;
                    affiliates = [];
                }
            };
            case (?existing) { existing };
        };
        
        let updatedStats: ProjectStats = {
            totalDonations = stats.totalDonations + donation.amount;
            donorCount = stats.donorCount + 1;
            matchingAmount = stats.matchingAmount;
            affiliates = switch (donation.affiliate) {
                case null { stats.affiliates };
                case (?aff) {
                    // Update affiliate contributions
                    var found = false;
                    let updated = Array.map<(Principal, Nat), (Principal, Nat)>(
                        stats.affiliates,
                        func((p, amt)) {
                            if (p == aff) {
                                found := true;
                                (p, amt + donation.amount)
                            } else {
                                (p, amt)
                            }
                        }
                    );
                    
                    if (not found) {
                        Array.append(updated, [(aff, donation.amount)])
                    } else {
                        updated
                    }
                }
            };
        };
        
        projectStats.put(donation.projectId, updatedStats);
    };
    
    private func getMedianPassportScore(address: Text) : Float {
        switch (passportScores.get(address)) {
            case null { 1.0 }; // Default score
            case (?scores) {
                if (scores.size() == 0) { return 1.0 };
                
                // Sort scores
                let sorted = Array.sort<GitCoinPassport>(
                    scores,
                    func(a, b) = Float.compare(a.score, b.score)
                );
                
                // Get median
                let mid = sorted.size() / 2;
                if (sorted.size() % 2 == 0 and sorted.size() > 1) {
                    (sorted[mid - 1].score + sorted[mid].score) / 2.0
                } else {
                    sorted[mid].score
                }
            };
        }
    };
    
    private func calculateAllProjectsSumOfSquares() : Float {
        var total = 0.0;
        // This would calculate for all projects
        // Simplified for now
        total
    };
    
    // Helper function to extract owner and repo from GitHub URL
    private func extractRepoInfo(url: Text) : ?{ owner: Text; repo: Text } {
        // Simple regex-like extraction for GitHub URLs
        // Format: https://github.com/owner/repo
        let parts = Text.split(url, #char('/'));
        let partsArray = Iter.toArray(parts);
        if (partsArray.size() >= 5 and Text.equal(partsArray[0], "https:") and Text.equal(partsArray[2], "github.com")) {
            ?{ owner = partsArray[3]; repo = partsArray[4] }
        } else {
            null
        }
    };
    
    // Helper function to fetch GitHub repository data using IC HTTP
    private func fetchGitHubRepoData(owner: Text, repo: Text) : async ?{
        name: Text;
        description: Text;
        owner: { login: Text };
        language: Text;
        stargazers_count: Nat;
        forks_count: Nat;
        topics: [Text];
        created_at: Text;
        updated_at: Text;
    } {
        // Make HTTP request to GitHub API
        let url = "https://api.github.com/repos/" # owner # "/" # repo;
        
        let request : HttpRequest = {
            url = url;
            method = "GET";
            body = "";
            headers = [
                ("User-Agent", "Science-Grants-Bot"),
                ("Accept", "application/vnd.github.v3+json")
            ];
            transform = null;
        };
        
        let ic : actor { http_request : HttpRequest -> async HttpResponse } = actor("aaaaa-aa");
        let response = await ic.http_request(request);
        
        if (response.status == 200) {
            // Parse JSON response
            // For now, return basic info since JSON parsing is complex in Motoko
            // In a full implementation, you would parse the JSON response
            ?{
                name = repo;
                description = "Repository data fetched from GitHub API";
                owner = { login = owner };
                language = "Unknown";
                stargazers_count = 0;
                forks_count = 0;
                topics = [];
                created_at = "2024-01-01T00:00:00Z";
                updated_at = "2024-01-01T00:00:00Z";
            }
        } else {
            // Fallback to basic info if API call fails
            ?{
                name = repo;
                description = "Repository submitted to Science Grants";
                owner = { login = owner };
                language = "Unknown";
                stargazers_count = 0;
                forks_count = 0;
                topics = [];
                created_at = "2024-01-01T00:00:00Z";
                updated_at = "2024-01-01T00:00:00Z";
            }
        }
    };
    
    // Helper function to determine project category
    private func determineCategory(description: Text, topics: [Text]) : { #science; #software } {
        let scienceKeywords = ["research", "science", "scientific", "study", "analysis", "experiment", "thesis", "paper", "academic", "scholarly", "mathematics", "physics", "chemistry", "biology", "medicine", "medical", "clinical", "laboratory", "lab"];
        
        // Check if any science keywords are in description or topics
        for (keyword in scienceKeywords.vals()) {
            if (Text.contains(description, #text(keyword))) {
                return #science;
            };
        };
        
        for (topic in topics.vals()) {
            for (keyword in scienceKeywords.vals()) {
                if (Text.contains(topic, #text(keyword))) {
                    return #science;
                };
            };
        };
        
        #software
    };
    
    // Query functions
    public query func getRoundConfig() : async ?RoundConfig {
        currentRound
    };
    
    public query func getProjectStats(projectId: Text) : async ?ProjectStats {
        projectStats.get(projectId)
    };
    
    public query func getMatchingPool(token: TokenType) : async Nat {
        switch (matchingPool.get(token)) {
            case null { 0 };
            case (?amount) { amount };
        }
    };
    
    // Submit a new project by GitHub URL
    public shared(msg) func submitProject(githubUrl: Text) : async Result.Result<Text, Text> {
        // Extract owner and repo from GitHub URL
        let repoInfo = extractRepoInfo(githubUrl);
        switch (repoInfo) {
            case null { #err("Invalid GitHub URL format") };
            case (?info) {
                // Fetch repository data from GitHub API
                let repoData = await fetchGitHubRepoData(info.owner, info.repo);
                switch (repoData) {
                    case null { #err("Failed to fetch repository data from GitHub") };
                    case (?data) {
                        let projectId = "project-" # Int.toText(Time.now());
                        
                        // Auto-determine category based on repository data
                        let category = determineCategory(data.description, data.topics);
                        
                        let project: Project = {
                            id = projectId;
                            githubUrl = githubUrl;
                            name = data.name;
                            description = data.description;
                            category = category;
                            owner = data.owner.login;
                            language = data.language;
                            stars = data.stargazers_count;
                            forks = data.forks_count;
                            topics = data.topics;
                            createdAt = data.created_at;
                            updatedAt = data.updated_at;
                            submittedAt = Time.now();
                            submittedBy = msg.caller;
                        };
                        
                        projects.put(projectId, project);
                        
                        // Initialize project stats
                        let initialStats: ProjectStats = {
                            totalDonations = 0;
                            donorCount = 0;
                            matchingAmount = 0;
                            affiliates = [];
                        };
                        projectStats.put(projectId, initialStats);
                        
                        #ok(projectId)
                    };
                };
            };
        };
    };
    
    // Get all projects
    public query func getProjects() : async [Project] {
        let projectArray = Buffer.Buffer<Project>(0);
        for ((_, project) in projects.entries()) {
            projectArray.add(project);
        };
        Buffer.toArray(projectArray)
    };
    
    // Get a specific project
    public query func getProject(projectId: Text) : async ?Project {
        projects.get(projectId)
    };
    
    // Wallet-related functions
    public shared(msg) func createUserWallet() : async Result.Result<WalletInfo, Text> {
        await wallet.createWallet()
    };
    
    public shared(msg) func getUserWallet() : async Result.Result<WalletInfo, Text> {
        await wallet.getWallet()
    };
    
    public shared(msg) func getWalletBalance() : async Result.Result<WalletTokens, Text> {
        await wallet.getBalance()
    };
    
    public shared(msg) func getWalletAccountId() : async Result.Result<WalletAccountIdentifier, Text> {
        await wallet.getAccountId()
    };
}