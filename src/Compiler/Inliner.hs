
-- inliner (vm, x) = 
--   let uses = map snd . filter ((<=1) . fst) . map (length &&& head) . group . sort . concatMap toList . Map.elems $ vm :: [String]
--       rep from to x = if x == from then to else x
--       once (f, t) = Map.map (fmap (rep f t))
--   in undefined -- (foldr once vm uses, x)

