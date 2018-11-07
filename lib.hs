-- todo: use where; remove spaces around :, remove parens
splitBy events pred =
  let foo (yes, no) (e : es) = if (pred e)
                                 then (foo (e : yes, no) es)
                                 else (foo (yes, e : no) es)
      foo results [] = results
   in foo ([], []) events
