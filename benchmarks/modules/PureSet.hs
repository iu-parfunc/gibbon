module PureSet where

    -- From [Comparing the performance of concurrent hash tables implemented in Haskell]
    --      (https://www.sciencedirect.com/science/article/pii/S0167642318302491)
    type Buckets = Array Int (TVar [ Int ] )
    type Locks = IORef Buckets
    data ThId = ThId ThreadId | Null
    data HTable = TH {
        buckets : : TVar Buckets,
        n_elements : : Tvar Int,
        len_tab : : TVar Int,
    }