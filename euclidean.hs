import Blades

basisElements :: [(Bitmap, (String, Double))]
basisElements = [ (basisVector n, ('e':show n, 1)) | n <- [1..3] ]