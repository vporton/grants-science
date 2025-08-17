import Principal "mo:core/Principal";
import Map "mo:core/Map";
import Text "mo:core/Text";
import Nat "mo:core/Nat";
import Int "mo:core/Int";
import Float "mo:core/Float";
import Time "mo:core/Time";
import Array "mo:core/Array";
import List "mo:core/List";
import Result "mo:core/Result";
import Iter "mo:core/Iter";
import Order "mo:core/Order";
import Nat64 "mo:core/Nat64";
import Blob "mo:core/Blob";


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
        #IS20: Text; // Token canister ID // FIXME: Why is this a Text? Should be a Principal.
    };

// Returns -1 if a < b, 0 if a == b, +1 if a > b
    private func compareTokenType(a: TokenType, b: TokenType) : Order.Order {
        switch (a, b) {
            case (#ICP, #ICP) { #equal };
            case (#ICP, #IS20 _) { #less }; // Define ICP < IS20
            case (#IS20 _, #ICP) { #greater };
            case (#IS20 id1, #IS20 id2) {
                Text.compare(id1, id2)
            };
        }
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
    // FIXME: Why `transient`?
    private transient var donations = Map.empty<Text, [DonationSpec]>();
    private transient var matchingPool = Map.empty<TokenType, Nat>();
    private transient var servers = Map.empty<Principal, ServerInfo>();
    private transient var projectStats = Map.empty<Text, ProjectStats>();
    private transient var projects = Map.empty<Text, Project>();
    private transient var passportScores = Map.empty<Text, [GitCoinPassport]>();
    private transient var withdrawals = Map.empty<Text, [(TokenType, Nat)]>();
    
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
                
                let current = switch (Map.get(matchingPool, compareTokenType, token)) {
                    case null { 0 };
                    case (?amt) { amt };
                };
                
                ignore Map.insert(matchingPool, compareTokenType, token, current + amount);
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
                
                ignore Map.insert(servers, Principal.compare, msg.caller, serverInfo);
                
                // Add to matching pool
                let current = switch (Map.get(matchingPool, compareTokenType, #ICP)) {
                    case null { 0 };
                    case (?amt) { amt };
                };
                ignore Map.insert(matchingPool, compareTokenType, #ICP, current + amount);
                
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
                    to = Blob.fromArray(Array.repeat<Nat8>(0, 32)); // Grants system account
                    amount = { e8s = Nat64.fromNat(spec.amount) };
                    memo = ?Nat64.fromNat(Int.abs(Time.now()));
                };
                
                let transferResult = await wallet.transfer(transferRequest);
                switch (transferResult) {
                    case (#err(e)) { return #err("Transfer failed: " # e) };
                    case (#ok(_)) { };
                };
                
                // Record donation
                let projectDonations = switch (Map.get(donations, Text.compare, spec.projectId)) {
                    case null { [] };
                    case (?existing) { existing };
                };
                
                let newDonations = Array.concat(projectDonations, [spec]);
                ignore Map.insert(donations, Text.compare, spec.projectId, newDonations);
                
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
        
        let scores = switch (Map.get(passportScores, Text.compare, address)) {
            case null { [] };
            case (?existing) { existing };
        };
        
        ignore Map.insert(passportScores, Text.compare, address, Array.concat(scores, [passport]));
        #ok("Passport score submitted")
    };
    
    // Calculate quadratic matching
    public func calculateMatching(projectId: Text) : async Nat {
        switch (currentRound) {
            case null { 0 };
            case (?round) {
                let projectDonations = switch (Map.get(donations, Text.compare, projectId)) {
                    case null { return 0 };
                    case (?d) { d };
                };
                
                // Group donations by donor and calculate square roots
                let donorTotals = Map.empty<Principal, Float>();
                
                for (donation in projectDonations.vals()) {
                    let donor = Principal.fromText(donation.projectId); // This should be donor address
                    let current = switch (Map.get(donorTotals, Principal.compare, donor)) {
                        case null { 0.0 };
                        case (?amt) { amt };
                    };
                    
                    // Get passport score
                    let score = getMedianPassportScore(Principal.toText(donor));
                    ignore Map.insert(donorTotals, Principal.compare, donor, current + Float.fromInt(donation.amount) * score);
                };
                
                // Calculate sum of square roots
                var sumOfSqrts = 0.0;
                for ((_, amount) in Map.entries(donorTotals)) {
                    sumOfSqrts += Float.sqrt(amount);
                };
                
                // Square the sum
                let matchingAmount = sumOfSqrts ** 2;
                
                // Get total matching pool for token type
                let totalPool = switch (Map.get(matchingPool, compareTokenType, #ICP)) {
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
                label l for ((projectId, _) in Map.entries(donations)) {
                    let matching = await calculateMatching(projectId);
                    let stats = switch (Map.get(projectStats, Text.compare, projectId)) {
                        case (?s) { s };
                        case null { continue l };
                    };
                    
                    // Calculate after tax and affiliate fees
                    let totalAmount = stats.totalDonations + matching;
                    let afterTax = totalAmount * (100 - round.worldScienceDAOTax) / 100;
                    
                    // Store withdrawal allowance
                    let current = switch (Map.get(withdrawals, Text.compare, projectId)) {
                        case (?w) { w };
                        case null { [] };
                    };
                    
                    ignore Map.insert(withdrawals, Text.compare, projectId, Array.concat(current, [(#ICP, afterTax)]));
                };
                
                #ok("Distributions calculated")
            };
            case null { #err("No active round") };
        }
    };
    
    // Withdraw funds
    public shared(msg) func withdraw(projectId: Text) : async Result.Result<Nat, Text> {
        switch (Map.get(withdrawals, Text.compare, projectId)) {
            case null { #err("No funds to withdraw") };
            case (?funds) {
                var total = 0;
                for ((_, amount) in funds.vals()) {
                    total += amount;
                };
                
                // Clear withdrawals
                ignore Map.delete(withdrawals, Text.compare, projectId);
                
                // In real implementation, transfer funds here
                #ok(total)
            };
        }
    };
    
    // Helper functions
    private func updateProjectStats(donation: DonationSpec) {
        let stats = switch (Map.get(projectStats, Text.compare, donation.projectId)) {
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
                        Array.concat(updated, [(aff, donation.amount)])
                    } else {
                        updated
                    }
                }
            };
        };
        
        ignore Map.insert(projectStats, Text.compare, donation.projectId, updatedStats);
    };
    
    private func getMedianPassportScore(address: Text) : Float {
        switch (Map.get(passportScores, Text.compare, address)) {
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
        Map.get(projectStats, Text.compare, projectId)
    };
    
    public query func getMatchingPool(token: TokenType) : async Nat {
        switch (Map.get(matchingPool, compareTokenType, token)) {
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
                        
                        ignore Map.insert(projects, Text.compare, projectId, project);
                        
                        // Initialize project stats
                        let initialStats: ProjectStats = {
                            totalDonations = 0;
                            donorCount = 0;
                            matchingAmount = 0;
                            affiliates = [];
                        };
                        ignore Map.insert(projectStats, Text.compare, projectId, initialStats);
                        
                        #ok(projectId)
                    };
                };
            };
        };
    };
    
    // Get all projects
    public query func getProjects() : async [Project] {
        let projectArray = List.empty<Project>();
        for ((_, project) in Map.entries(projects)) {
            List.add(projectArray, project);
        };
        List.toArray(projectArray)
    };
    
    // Get a specific project
    public query func getProject(projectId: Text) : async ?Project {
        Map.get(projects, Text.compare, projectId)
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