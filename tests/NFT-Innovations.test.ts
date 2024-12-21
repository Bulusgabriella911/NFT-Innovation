import { describe, it, expect, beforeEach } from "vitest";

// Mocking the blockchain state for the Clarity contract
type NFTState = {
  lastTokenId: number;
  tokenOwners: Map<string, number>;
  activityLevels: Map<string, number>;
  evolutionStages: Map<string, number>;
};

let nftState: NFTState;

// Helper function to reset the NFT state
const resetNFTState = () => {
  nftState = {
    lastTokenId: 0,
    tokenOwners: new Map(),
    activityLevels: new Map(),
    evolutionStages: new Map(),
  };
};

// Mocked contract functions
const mintSubscription = (wallet: string) => {
  if (nftState.tokenOwners.has(wallet)) {
    return { err: 101 }; // ERR_NFT_EXISTS
  }
  const newTokenId = nftState.lastTokenId + 1;
  nftState.tokenOwners.set(wallet, newTokenId);
  nftState.activityLevels.set(wallet, 1);
  nftState.evolutionStages.set(wallet, 1);
  nftState.lastTokenId = newTokenId;
  return { ok: newTokenId };
};

const recordActivity = (wallet: string) => {
  const currentLevel = nftState.activityLevels.get(wallet) || 0;
  nftState.activityLevels.set(wallet, currentLevel + 1);
  evolveNFT(wallet);
  return { ok: true };
};

const evolveNFT = (wallet: string) => {
  const activity = nftState.activityLevels.get(wallet) || 0;
  const currentStage = nftState.evolutionStages.get(wallet) || 1;

  if (activity >= currentStage * 5 && currentStage < 5) {
    nftState.evolutionStages.set(wallet, currentStage + 1);
    return { ok: true };
  }
  return { ok: false };
};

const getActivityLevel = (wallet: string) => {
  return nftState.activityLevels.get(wallet) || 0;
};

const getEvolutionStage = (wallet: string) => {
  return nftState.evolutionStages.get(wallet) || 0;
};

// Tests
describe("NFT Subscription Contract", () => {
  beforeEach(() => {
    resetNFTState();
  });

  it("should mint a new subscription NFT for a wallet", () => {
    const result = mintSubscription("wallet_1");
    expect(result.ok).toBe(1);
    expect(nftState.tokenOwners.get("wallet_1")).toBe(1);
    expect(getActivityLevel("wallet_1")).toBe(1);
    expect(getEvolutionStage("wallet_1")).toBe(1);
  });

  it("should not mint a new NFT for a wallet that already owns one", () => {
    mintSubscription("wallet_1");
    const result = mintSubscription("wallet_1");
    expect(result.err).toBe(101);
  });

  it("should record activity for a wallet and update activity level", () => {
    mintSubscription("wallet_1");
    recordActivity("wallet_1");
    expect(getActivityLevel("wallet_1")).toBe(2);
  });

  it("should evolve the NFT stage when activity threshold is met", () => {
    mintSubscription("wallet_1");
    for (let i = 0; i < 5; i++) {
      recordActivity("wallet_1");
    }
    expect(getEvolutionStage("wallet_1")).toBe(2);
  });

  it("should not evolve the NFT stage beyond the maximum stage", () => {
    mintSubscription("wallet_1");
    for (let i = 0; i < 25; i++) {
      recordActivity("wallet_1");
    }
    expect(getEvolutionStage("wallet_1")).toBe(5); // Max stage
  });

  it("should return the correct activity level for a wallet", () => {
    mintSubscription("wallet_1");
    recordActivity("wallet_1");
    recordActivity("wallet_1");
    expect(getActivityLevel("wallet_1")).toBe(3);
  });

  it("should return the correct evolution stage for a wallet", () => {
    mintSubscription("wallet_1");
    for (let i = 0; i < 10; i++) {
      recordActivity("wallet_1");
    }
    expect(getEvolutionStage("wallet_1")).toBe(3);
  });
});
