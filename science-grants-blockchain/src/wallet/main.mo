import Principal "mo:core/Principal";
import Map "mo:core/Map";
import Text "mo:core/Text";
import Nat64 "mo:core/Nat64";
import Nat8 "mo:core/Nat8";
import Array "mo:core/Array";
import Result "mo:core/Result";
import Blob "mo:core/Blob";



// Wallet canister
persistent actor Wallet {
    // Types
    public type SubAccount = Blob;
    public type AccountIdentifier = Blob;
    public type Tokens = {
        e8s : Nat64;
    };
    public type TransferArgs = {
        memo : Nat64;
        amount : Tokens;
        fee : Tokens;
        from_subaccount : ?SubAccount;
        to : AccountIdentifier;
        created_at_time : ?Nat64;
    };
    public type TransferResult = {
        #Ok : Nat64;
        #Err : TransferError;
    };
    public type TransferError = {
        #BadFee : { expected_fee : Tokens };
        #BadBurn : { min_burn_amount : Tokens };
        #InsufficientFunds : { balance : Tokens };
        #TooOld;
        #CreatedInFuture : { ledger_time : Nat64 };
        #Duplicate : { duplicate_of : Nat64 };
        #TemporarilyUnavailable;
        #GenericError : { error_code : Nat64; message : Text };
    };
    
    public type WalletInfo = {
        principal : Principal;
        subaccount : SubAccount;
        balance : Tokens;
        accountId : AccountIdentifier;
    };
    
    public type TransferRequest = {
        to : AccountIdentifier;
        amount : Tokens;
        memo : ?Nat64;
    };
    
    // State
    private transient var userWallets = Map.empty<Principal, WalletInfo>();
    
    // ICP Ledger canister ID (mainnet)
    private transient let ICP_LEDGER_CANISTER_ID = "ryjl3-tyaaa-aaaaa-aaaba-cai";
    
    // ICP Ledger interface types
    private type LedgerAccountIdentifier = Blob;
    private type LedgerSubAccount = Blob;
    private type LedgerBlockIndex = Nat64;
    private type LedgerMemo = Nat64;
    private type LedgerTimeStamp = Nat64;
    private type LedgerTokens = {
        e8s : Nat64;
    };
    private type LedgerTransferArgs = {
        memo : LedgerMemo;
        amount : LedgerTokens;
        fee : LedgerTokens;
        from_subaccount : ?LedgerSubAccount;
        to : LedgerAccountIdentifier;
        created_at_time : ?LedgerTimeStamp;
    };
    private type LedgerTransferResult = {
        #Ok : LedgerBlockIndex;
        #Err : LedgerTransferError;
    };
    private type LedgerTransferError = {
        #BadFee : { expected_fee : LedgerTokens };
        #BadBurn : { min_burn_amount : LedgerTokens };
        #InsufficientFunds : { balance : LedgerTokens };
        #TooOld;
        #CreatedInFuture : { ledger_time : LedgerTimeStamp };
        #Duplicate : { duplicate_of : LedgerBlockIndex };
        #TemporarilyUnavailable;
        #GenericError : { error_code : Nat64; message : Text };
    };
    
    // ICP Ledger actor interface
    private type Ledger = actor {
        account_balance : shared query { account : LedgerAccountIdentifier } -> async { e8s : Nat64 };
        transfer : shared LedgerTransferArgs -> async LedgerTransferResult;
    };
    
    // ICP Ledger actor
    private transient let icpLedger : Ledger = actor(ICP_LEDGER_CANISTER_ID);
    
    // Create or get user wallet
    public shared(msg) func createWallet() : async Result.Result<WalletInfo, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?existing) { #ok(existing) };
            case null {
                // Generate subaccount for user
                let subaccount = generateSubaccount(caller);
                let accountId = accountIdentifier(caller, ?subaccount);
                
                let walletInfo : WalletInfo = {
                    principal = caller;
                    subaccount = subaccount;
                    balance = { e8s = 0 };
                    accountId = accountId;
                };
                
                ignore Map.insert(userWallets, Principal.compare, caller, walletInfo);
                #ok(walletInfo)
            };
        }
    };
    
    // Get user wallet info
    public shared query(msg) func getWallet() : async Result.Result<WalletInfo, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?wallet) { #ok(wallet) };
            case null { #err("Wallet not found. Please create a wallet first.") };
        }
    };
    
    // Get wallet balance
    public shared(msg) func getBalance() : async Result.Result<Tokens, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?wallet) {
                // Query ICP ledger for actual balance
                let accountId = wallet.accountId;
                let balance = await icpLedger.account_balance({ account = accountId });
                
                // Update stored balance
                let updatedWallet = {
                    principal = wallet.principal;
                    subaccount = wallet.subaccount;
                    balance = balance;
                    accountId = wallet.accountId;
                };
                ignore Map.insert(userWallets, Principal.compare, caller, updatedWallet);
                
                #ok(balance)
            };
            case null { #err("Wallet not found. Please create a wallet first.") };
        }
    };
    
    // Transfer ICP from user's wallet to another account
    public shared(msg) func transfer(request : TransferRequest) : async Result.Result<Nat64, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?wallet) {
                // Check if user has sufficient balance
                let currentBalance = await getBalance();
                switch (currentBalance) {
                    case (#err(e)) { return #err(e) };
                    case (#ok(balance)) {
                        if (balance.e8s < request.amount.e8s) {
                            return #err("Insufficient balance");
                        };
                        
                        // Prepare transfer args
                        let transferArgs : TransferArgs = {
                            memo = switch (request.memo) {
                                case (?memo) { memo };
                                case null { 0 };
                            };
                            amount = request.amount;
                            fee = { e8s = 10000 }; // Standard ICP transfer fee
                            from_subaccount = ?wallet.subaccount;
                            to = request.to;
                            created_at_time = null;
                        };
                        
                        // Execute transfer
                        let result = await icpLedger.transfer(transferArgs);
                        
                        switch (result) {
                            case (#Ok(blockIndex)) {
                                // Update balance
                                let newBalance = { e8s = balance.e8s - request.amount.e8s - 10000 };
                                let updatedWallet = {
                                    principal = wallet.principal;
                                    subaccount = wallet.subaccount;
                                    balance = newBalance;
                                    accountId = wallet.accountId;
                                };
                                ignore Map.insert(userWallets, Principal.compare, caller, updatedWallet);
                                
                                #ok(blockIndex)
                            };
                            case (#Err(error)) {
                                let errorMsg = switch (error) {
                                    case (#BadFee(e)) { "Bad fee: expected " # Nat64.toText(e.expected_fee.e8s) };
                                    case (#BadBurn(e)) { "Bad burn: minimum " # Nat64.toText(e.min_burn_amount.e8s) };
                                    case (#InsufficientFunds(e)) { "Insufficient funds: balance " # Nat64.toText(e.balance.e8s) };
                                    case (#TooOld) { "Transfer too old" };
                                    case (#CreatedInFuture(e)) { "Created in future: " # Nat64.toText(e.ledger_time) };
                                    case (#Duplicate(e)) { "Duplicate transfer: " # Nat64.toText(e.duplicate_of) };
                                    case (#TemporarilyUnavailable) { "Temporarily unavailable" };
                                    case (#GenericError(e)) { "Generic error: " # e.message };
                                };
                                #err(errorMsg)
                            };
                        }
                    };
                }
            };
            case null { #err("Wallet not found. Please create a wallet first.") };
        }
    };
    
    // Transfer ICP to user's wallet (for donations)
    public shared(msg) func transferToWallet(amount : Tokens) : async Result.Result<Nat64, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?wallet) {
                // This would typically be called by the grants system
                // The grants system would transfer ICP from its account to the user's wallet
                // For now, we'll just update the balance (in real implementation, this would be a real transfer)
                
                let currentBalance = await getBalance();
                switch (currentBalance) {
                    case (#err(e)) { return #err(e) };
                    case (#ok(balance)) {
                        let newBalance = { e8s = balance.e8s + amount.e8s };
                        let updatedWallet = {
                            principal = wallet.principal;
                            subaccount = wallet.subaccount;
                            balance = newBalance;
                            accountId = wallet.accountId;
                        };
                        ignore Map.insert(userWallets, Principal.compare, caller, updatedWallet);
                        
                        #ok(0) // Mock block index
                    };
                }
            };
            case null { #err("Wallet not found. Please create a wallet first.") };
        }
    };
    
    // Get account identifier for deposits
    public shared query(msg) func getAccountId() : async Result.Result<AccountIdentifier, Text> {
        let caller = msg.caller;
        
        switch (Map.get(userWallets, Principal.compare, caller)) {
            case (?wallet) { #ok(wallet.accountId) };
            case null { #err("Wallet not found. Please create a wallet first.") };
        }
    };
    
    // Helper functions
    private func generateSubaccount(principal : Principal) : SubAccount {
        // Generate a deterministic subaccount based on the principal
        let principalBytes = Blob.toArray(Principal.toBlob(principal));
        let hash = Array.foldLeft<Nat8, Nat8>(
            principalBytes,
            0,
            func(acc, byte) = acc ^ byte
        );
        
        // Create a 32-byte subaccount
        let subaccountArray = Array.tabulate<Nat8>(32, func(i) = if (i == 0) { Nat8.fromNat(Nat8.toNat(hash)) } else { 0 });
        
        Blob.fromArray(subaccountArray)
    };
    
    private func accountIdentifier(principal : Principal, subaccount : ?SubAccount) : AccountIdentifier {
        // This is a simplified version. In production, you'd use the proper account identifier derivation
        let principalBlob = Principal.toBlob(principal);
        let subaccountBlob = switch (subaccount) {
            case (?sub) { sub };
            case null { Blob.fromArray(Array.tabulate<Nat8>(32, func(i) = 0)) };
        };
        
        // Concatenate principal and subaccount
        let principalArray = Blob.toArray(principalBlob);
        let subaccountArray = Blob.toArray(subaccountBlob);
        
        Blob.fromArray(Array.concat(principalArray, subaccountArray))
    };
    
    // System functions for upgrade
    system func preupgrade() {
        // Save state before upgrade
    };
    
    system func postupgrade() {
        // Restore state after upgrade
    };
}
