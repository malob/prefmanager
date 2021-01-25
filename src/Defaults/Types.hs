module Defaults.Types where

import Data.Map (Map)
import Data.Text (Text)
import Patience.Map (Delta)
import Text.XML.Plist (PlObject)

-- | Name of a domain, e.g., @NSGlobalDomain@, @com.apple.finder@, etc.
newtype DomainName = DomainName Text deriving (Eq, Ord, Show)

type Key = String

-- | Representation of the settings of a domain.
type Domain = Map Key PlObject

-- | Map of domains.
type Domains = Map DomainName Domain

-- | Map representing the change of the values of keys of a domain.
newtype DomainDiff = DomainDiff (Map Key (Delta PlObject)) deriving (Eq, Ord, Show)
