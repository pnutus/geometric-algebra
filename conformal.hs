import Blades

basisElements :: [(Bitmap, (String, Double))]
basisElements = (basisVector 1, ("no", 3000)) :
      [ (basisVector (n + 1), ('e':show n, 1)) | n <- [1..3] ]
      ++ [(basisVector 5, ("∞", 5000))]