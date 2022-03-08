module Logic.Damage.Attribute where

data Stat = Attack | Defence | SpAttack | SpDefence deriving (Eq,Show,Ord,Enum,Bounded)

data Counterparty = User | Target deriving (Eq,Show,Ord,Enum,Bounded)

data Category = Special | Physical deriving (Eq,Show,Ord,Enum,Bounded)

data CriticalHit = NonCritical | Critical deriving (Eq,Show,Ord,Enum,Bounded)

