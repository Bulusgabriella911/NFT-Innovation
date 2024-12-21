# NFT Subscription Contract

## Project Overview
The **NFT Subscription Contract** is an innovative smart contract that enables users to mint subscription-based NFTs, track their activity levels, and evolve their NFTs through usage. This contract incentivizes user engagement by introducing gamified progression mechanics tied to on-chain activity.

### Key Features
1. **NFT Minting**: Users can mint a unique subscription NFT, which tracks their activity within the ecosystem.
2. **Activity Tracking**: Each wallet’s activity level increases as users engage with the platform.
3. **NFT Evolution**: NFTs evolve into higher stages based on the wallet’s activity level, unlocking a progression system with up to five stages.
4. **Access Control**: Prevents duplicate NFT minting for the same wallet.

## Contract Functions

### Public Functions

#### `mint-subscription`
- **Description**: Mints a subscription NFT for the caller’s wallet.
- **Parameters**: None.
- **Returns**: The newly minted token ID or an error if the wallet already owns an NFT.
- **Errors**:
  - `ERR_NFT_EXISTS (101)`: Wallet already owns an NFT.

#### `record-activity`
- **Description**: Increases the caller’s activity level and evaluates if the NFT should evolve to the next stage.
- **Parameters**: None.
- **Returns**: `true` on successful activity recording.

### Private Functions

#### `evolve-nft`
- **Description**: Evolves the NFT’s stage if the activity level reaches the required threshold and the maximum stage hasn’t been reached.
- **Parameters**: None (operates on the caller’s wallet).
- **Returns**: `true` if the NFT evolves, `false` otherwise.

### Read-Only Functions

#### `get-activity-level`
- **Description**: Retrieves the activity level for a specified wallet.
- **Parameters**:
  - `owner (principal)`: The wallet address.
- **Returns**: The activity level of the specified wallet.

#### `get-evolution-stage`
- **Description**: Retrieves the evolution stage of the NFT for a specified wallet.
- **Parameters**:
  - `owner (principal)`: The wallet address.
- **Returns**: The evolution stage of the specified wallet’s NFT.

## Evolution Stages
- The NFT evolves through five stages.
- Evolution thresholds are based on the following formula:
  ```
  Activity >= Current Stage * 5
  ```
- Maximum stage: 5.

## Testing
The project includes a comprehensive test suite written with **Vitest** to validate the contract’s functionality.

### Test Scenarios
1. **Minting NFTs**
   - Successfully minting a subscription NFT.
   - Preventing duplicate minting for the same wallet.

2. **Recording Activity**
   - Incrementing the activity level for a wallet.
   - Evolving NFTs upon reaching activity thresholds.
   - Ensuring NFTs do not evolve beyond the maximum stage.

3. **Read-Only Functions**
   - Verifying correct retrieval of activity levels and evolution stages.

## How to Use
1. Deploy the contract to your blockchain environment.
2. Call `mint-subscription` to mint an NFT for your wallet.
3. Use `record-activity` to increase your activity level and evolve your NFT.
4. Check your progress with `get-activity-level` and `get-evolution-stage`.

## Errors
| Error Code | Description                   |
|------------|-------------------------------|
| `100`      | Not authorized (reserved).   |
| `101`      | Wallet already owns an NFT. |

## Future Improvements
- Introducing rewards for higher NFT stages.
- Adding customization options for evolved NFTs.
- Implementing activity tracking across multiple dApps.

## License
This project is licensed under the MIT License. See the LICENSE file for details.

