## Mappable
**Version - 0.1.0.0**

[Mappable](https://github.com/Sinha-Ujjawal/Mappable) is a Haskell Library to make haskell structures like Sets and Map 
mappable which are not really functors. It also provides a Bag data structure that is implemented using Data.Map.Strict.

---

## Usage
#### Making Mappable Instance
```haskell
instance Functor Collection where
    --fmap :: (a -> b) -> Collection a -> Collection b

instance (Constraint a, Constraint b) => Mappable Collection where
    --map :: (a -> b) -> Collection a -> Collection b

-- eg-
instance Mappable => Maybe a b where
    map = fmap

instance (Ord a, Ord b) Mappable => Set a b where
  -- map
```

The only difference between [Functor](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Functor) and Mappable is that mappable allows you to put arbitrary constraint on `a` and `b`.

---

## License & Copyrights
© [Sinha, Ujjawal](https://github.com/Sinha-Ujjawal)

Licensed under [MIT LICENSE](LICENSE)
