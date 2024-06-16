My plutus-toy project
---
A collection of random plutus scripts 

## What is this
A play-ground where I can quickly prototype plutus development in Haskell

## What do I do with it
Develop 
Deploy

### Develop
Start from:

``` sh
nix develop
cabal build all
```

### Cardano-node
Current version of node is 8.93. See [flake.nix](./flake.nix) for detail
To Bring cardano-node up:

``` sh
./start-cardano-node preview
tail -f ./var/8.93/preview/log/cardano-node-preview.log
```
To query the tip:

``` sh
cardano-cli query tip --testnet-magic 2 --socket-path ./var/8.93/preview/cardano-node-preview.socket
```



### References
- [Generate keys/wallet and funding address](https://developers.cardano.org/docs/native-tokens/minting-nfts/#generate-keys-and-address)
- [plutus-user guide](https://intersectmbo.github.io/plutus/master/docs/)
- [all plutus-haddocs](https://intersectmbo.github.io/plutus/master/)
- [aiken eutxo crash course](https://aiken-lang.org/fundamentals/eutxo)
