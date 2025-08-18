# 🛃 Customs Duty DAO

> 🗳️ Democratizing trade policy through blockchain governance

A decentralized autonomous organization (DAO) that gives importers and trade stakeholders a voice in customs duty and tariff decisions through token-weighted voting.

## 🌟 Features

- **🪙 Token-Based Governance**: Stake customs tokens (CDT) to participate in DAO decisions
- **📝 Proposal System**: Create and vote on customs duty rate changes
- **💰 Treasury Management**: Transparent fund management for lobbying and compliance programs
- **⚖️ Democratic Voting**: Token-weighted voting system ensures fair representation
- **🔒 Secure Staking**: Lock tokens to gain voting power and proposal rights

## 🚀 Quick Start

### Prerequisites
- [Clarinet](https://docs.hiro.so/stacks/clarinet) installed
- Node.js for testing

### Installation
```bash
git clone https://github.com/your-username/Customs-Duty-DAO.git
cd Customs-Duty-DAO
clarinet check
```

## 📖 Usage

### 🏁 Initialize the Contract
```clarity
(contract-call? .customs-duty-dao initialize u100000000)
```

### 🔐 Stake Tokens
Before participating in governance, stake your CDT tokens:
```clarity
(contract-call? .customs-duty-dao stake-tokens u1000000)
```

### 📋 Create a Proposal
Submit a proposal for customs duty changes:
```clarity
(contract-call? .customs-duty-dao create-proposal 
  "Reduce Electronics Tariff"
  "Lower duty rate on consumer electronics to boost trade"
  u15
  "HS8471")
```

### 🗳️ Vote on Proposals
Cast your vote using staked tokens:
```clarity
(contract-call? .customs-duty-dao vote-on-proposal u1 true u500000)
```

### 💵 Contribute to Treasury
Support DAO operations:
```clarity
(contract-call? .customs-duty-dao contribute-to-treasury u100000)
```

## 🔧 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `initialize` | 🎯 Initialize contract with token supply | `initial-supply: uint` |
| `stake-tokens` | 🔒 Stake tokens for voting power | `amount: uint` |
| `unstake-tokens` | 🔓 Withdraw staked tokens | `amount: uint` |
| `create-proposal` | 📝 Submit new proposal | `title, description, target-duty-rate, commodity-code` |
| `vote-on-proposal` | 🗳️ Vote on active proposal | `proposal-id: uint, vote: bool, amount: uint` |
| `execute-proposal` | ⚡ Execute passed proposal | `proposal-id: uint` |
| `contribute-to-treasury` | 💰 Add funds to treasury | `amount: uint` |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-proposal` | 📊 Get proposal details | Proposal data |
| `get-member-stake` | 👤 Get member's stake info | Stake details |
| `get-treasury-balance` | 💎 Get current treasury funds | Balance amount |
| `get-voting-parameters` | ⚙️ Get voting configuration | Parameters object |

## 🎯 Governance Parameters

- **Minimum Proposal Stake**: 1,000,000 CDT
- **Voting Period**: 1,440 blocks (~10 days)
- **Quorum Threshold**: 5,000,000 CDT
- **Token Decimals**: 6

## 🏗️ Architecture

The DAO operates through four main components:

1. **🪙 Token System**: CDT tokens for governance participation
2. **📋 Proposal Management**: Create, vote, and execute proposals
3. **💰 Treasury**: Decentralized fund management
4. **🔒 Staking Mechanism**: Token locking for voting rights

## 🧪 Testing

Run the test suite:
```bash
npm install
npm test
```

## 🛡️ Security Features

- ✅ Owner-only administrative functions
- ✅ Voting period enforcement
- ✅ Quorum requirements
- ✅ Stake validation
- ✅ Duplicate vote prevention

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

---

*Built with ❤️ for decentralized trade governance*
