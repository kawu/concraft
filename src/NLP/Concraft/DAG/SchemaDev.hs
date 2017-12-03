{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Observation schemas for Concraft.
--
-- The main differences w.r.t. `Schema` are:
-- * The aim of both modules is to define a language of observation schemas.
--   However, the focus is slighly different here: we want a language which will
--   be easy to use in an external configuration files (cf. Dhall), i.e., we
--   want to define observation schemas externally and, then, interpret them to
--   Haskell expressions.
-- * We don't use monad-ox (TODO: to re-conisider?)


module NLP.Concraft.DAG.SchemaDev
(
) where


import qualified Data.Map.Strict as M


------------------------------
-- Basic Types
------------------------------


-- | Observation type (before: index). This allows to distinguish two identical
-- observations, based on their types.
type Ty = T.Text


-- | An observation consists of an index (list of ints) and an actual
-- observation value.
-- type Ob = ([Int], T.Text)
type Ob = T.Text


-- | An observation map, i.e., what is assigned to the individual DAG edges as a
-- result of schematization.
type ObMap = M.Map Ty Ob


------------------------------------------------------------
-- Schema
--
--   * Whatever a schema is, it is probably a Monoid!
--     For instance:
--
--       orth [-1] <> orth [0, 1] = orth [-1, 0, 1]
--
--   * From this perspective, it might be sufficient to allow
--     atomic Schema expression such as:
--
--       orth 0
--       orth -1
--
--     which can be later combined using `<>`
--
--  * There are also more complex primitives as, for example, the prefix primitive:
--
--      prefix 2 0
--
--    where the first number represents the length of the prefix. But should it
--    really be a primitive with the same status as `orth`?  I'm not convinced,
--    since it could be easily a function which tranforms the result of another
--    expression.  Int this case, `prefix 2` is such a function, which tranforms
--    an `Ob` by taking its prefix.  Why not?
--
--  * From the previous point, one can deduce that, perhaps, a Schema should be a
--    `Functor`?
--
--  * We would like also to be able to combine two schemas, e.g.,
--
--      join (shape -1) (shape 0)
--
--  * In fact, it would be nice if the user were allowed to define his own
--    combinators, similar to the `join` above (the idea of the `join` being
--    that it glues together the two values).
--
--  * BOLD IDEA: why shouldn't we make our observations real numbers? This would
--    allow to nicely account for segmentation ambiguities: going left/right
--    from a given edge could be non-deterministic (with the weights distributed
--    evently between the different alternatives). Alternatively, we could rely
--    on the prior probabilities (coming from upstream models) of the individual
--    edges. That would be nice! And, of course, this would allow to account for
--    intrinsically numerical observations (e.g. association measures).
--
------------------------------------------------------------


-- | Not sure if this is really a `Schema` or rather an anonymouos schema element.
data Schema where
  -- The orthographic form, on a given (relative) position; at the moment,
  -- the only primitive schema
  Orth :: Int -> Schema
  -- Type (or name) a given schema
  Type :: Ty -> Schema -> Schema
  -- A functional transformation (a monomorphic functor?)
  Map :: (Ob -> Ob) -> Schema -> Schema
  -- Composition of two schemas (a product type?); the type of the result
  -- is equal to the type of the first schema argument (but this should be
  -- not relied on, I suppose)
  Compose :: (Ob -> Ob -> Ob) -> Schema -> Schema -> Schema
  -- Union of two schemas
  Union :: Schema -> Schema -> Schema


-- IDEA: you could distinguish named from anonymous schemas, as you did below?
-- Then, you could enforce that, e.g., only the anonymuous schemas can be
-- composed, but that only the named schemas can be unioned, etc.
-- But perhaps it would make more sense to just distinguish these two types:
-- named and anonymuous schemas.

-- QUESTION: now the question is, how to (i) make it possible to define such
-- schemas in Dhall, and (ii) make such schemas serializable? As for (ii), we
-- could perhaps serialize the Dhall description of the schema. Seems feasible.


-- -- | A boolean query to be evaluated over a node of a syntactic tree.
-- -- Type `n` stands for node type.
-- data Expr n a where
--   -- Mark the given node as interesting (can be used, for instance,
--   -- to mark lexically constrained nodes)
--   Mark :: Expr n 'Node
--   -- Simply a boolean value
--   B :: Bool -> Expr n a
--   -- Logical conjunction
--   And :: Expr n a -> Expr n a -> Expr n a
--   -- Logical disjunction
--   Or :: Expr n a -> Expr n a -> Expr n a
--   -- Does the current node satisfy the given predicate?
--   SatisfyNode :: (n -> Bool) -> Expr n 'Node
--   -- Like `SatisfyNode`, but works on trees; be aware that if you use `Satisfy`
--   -- over a tree to check some lexical constraints, the corresponding lexical
--   -- nodes will not be marked.
--   SatisfyTree :: (R.Tree n -> Bool) -> Expr n 'Tree
--   -- Run the expression over the current node
--   Current :: Expr n 'Node -> Expr n 'Tree
--   -- The current node has to be an immediate parent of a tree which
--   -- satisfied the given query/expression
--   Child :: (n -> Bool) -> Expr n 'Tree -> Expr n 'Tree
--   -- Check that the tree is non-branching
--   NonBranching :: Expr n 'Tree
