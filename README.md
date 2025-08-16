# 🔐 Escrowit - Escrow-as-a-Service Platform

> **Generic escrow builder for dApps on Stacks blockchain** 🚀

Escrowit is a decentralized escrow service that enables secure peer-to-peer transactions with built-in arbitration. Perfect for marketplaces, freelance platforms, and any dApp requiring trustless transactions.

## ✨ Features

- 🛡️ **Secure Escrow Creation** - Lock funds safely between parties
- ⚖️ **Built-in Arbitration** - Third-party dispute resolution
- ⏰ **Time-based Expiration** - Automatic refunds for expired escrows
- 💰 **Platform Fee System** - Sustainable revenue model
- 📊 **User Dashboard** - Track all your escrows in one place
- 🔍 **Transparent Operations** - All transactions on-chain

## 🏗️ Core Functions

### Creating an Escrow
```clarity
(contract-call? .Escrowit create-escrow seller-address arbiter-address amount-in-ustx duration-in-blocks "Description")
```

### Releasing Funds (Buyer/Arbiter)
```clarity
(contract-call? .Escrowit release-funds escrow-id)
```

### Refunding Buyer (Seller/Arbiter)
```clarity
(contract-call? .Escrowit refund-buyer escrow-id)
```

### Claiming Expired Escrow (Buyer)
```clarity
(contract-call? .Escrowit claim-expired-escrow escrow-id)
```

## 📋 How It Works

1. **🎯 Create Escrow**: Buyer creates escrow with seller and arbiter addresses
2. **💸 Fund Escrow**: STX tokens are locked in the contract
3. **🤝 Complete Transaction**: Either buyer or arbiter can release funds to seller
4. **🔄 Handle Disputes**: Seller or arbiter can refund buyer if needed
5. **⏱️ Expiration Safety**: Buyer can claim funds back after expiration

## 🔧 Setup & Deployment

### Prerequisites
- Clarinet installed
- Stacks wallet for testing

### Installation
```bash
git clone <your-repo>
cd escrowit
clarinet check
```

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy --testnet
```

## 📊 Contract Details

- **Platform Fee**: 2.5% (250 basis points) - adjustable by contract owner
- **Maximum User Escrows**: 100 per user
- **Escrow States**: `active`, `completed`, `refunded`, `expired`

## 🎮 Usage Examples

### For Marketplaces
Perfect for e-commerce platforms where buyers need protection and sellers need payment guarantees.

### For Freelance Platforms
Ideal for service-based transactions where work completion needs verification.

### For P2P Trading
Great for any peer-to-peer exchange requiring trustless transactions.

## 🔍 Read-Only Functions

- `get-escrow` - Retrieve escrow details
- `get-user-escrows` - Get all escrows for a user
- `is-escrow-expired` - Check if escrow has expired
- `calculate-platform-fee` - Calculate fee for given amount
- `can-release-funds` - Check if caller can release funds
- `can-refund` - Check if caller can issue refund

## 🛡️ Security Features

- ✅ Authorization checks for all operations
- ✅ Balance validation before transfers
- ✅ Status verification for state changes
- ✅ Expiration time enforcement
- ✅ Double-spending prevention

## 🤝 Contributing

We welcome contributions! Please feel free to submit issues and pull requests.

## 📄 License

MIT License - Build amazing things! 🌟

---


