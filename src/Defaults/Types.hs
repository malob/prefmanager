module Defaults.Types where

import Patience.Map (Delta)
import Text.XML.Plist (PlObject)

-- | Name of a domain, e.g., @NSGlobalDomain@, @com.apple.finder@, etc.
newtype DomainName = DomainName Text deriving (Eq, Ord, Show)

type Key = String

-- | Representation of the settings of a domain.
newtype Domain = Domain (Map Key PlObject) deriving (Eq, Ord, Show)

-- | Map of domains.
newtype Domains = Domains (Map DomainName Domain) deriving (Eq, Ord, Show)

-- | Map representing the change of the values of keys of a domain.
newtype DomainDiff = DomainDiff (Map Key (Delta PlObject)) deriving (Eq, Ord, Show)
