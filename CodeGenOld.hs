--Types


type Loc = Hex
type Val = Hex

data Address = Adr {loc :: Loc, val :: Val} deriving (Eq)

data Register = Reg Val deriving (Eq, Show)

data Memory = Memory {addresses :: [Address], xReg :: Register, yReg :: Register, acc :: Register} deriving (Eq)

instance Show Address where
    show adr@(Adr str@('$':xs) val) = str++":    "++val
    show adr@(Adr str val) = '$':(replicate (4 - (length str)) '0')++str++":    "++val

instance Ord Address where
    compare a@(Adr numA _) b@(Adr numB _) = compare (hexToDec (drop 1 numA)) (hexToDec (drop 1 numB))




--Helper Functions
getLoc :: Address -> Hex
getLoc adr@(Adr str _) = (replicate (4 - (length str)) '0')++(loc adr)


instance Show Memory where
    show mem@(Memory adrs x@(Reg vx) y@(Reg vy) a@(Reg va)) =
        "X:    "++vx++"\nY:    "++vy++"\nA:    "++va++"\n"++(concat (map ((++"\n").(show)) (filter ((/="ZZ").(val)) adrs)))