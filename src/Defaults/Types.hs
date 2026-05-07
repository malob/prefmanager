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

-- | A renderable watch-loop event for one domain.
data WatchEvent
  = EventChanged DomainDiff
    -- ^ existing domain whose keys changed
  | EventAdded   [Key] Int
    -- ^ domain that newly appeared post-startup. The keys are the
    -- post-filter list to display; the 'Int' is the total number of
    -- keys before filtering, so the renderer can honestly show
    -- "3 of 200 keys" when most were filtered out.
  | EventRemoved [Key] Int
    -- ^ domain that disappeared from @defaults domains@ mid-watch. Same
    -- shape as 'EventAdded', but the keys are used only for counting
    -- (the renderer prints "(Domain removed, N keys)" and does not
    -- list them). Fires once on first vanish; subsequent polls where
    -- the domain stays missing are silent.
