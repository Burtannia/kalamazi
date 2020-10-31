{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Colour where

import Yesod.ClassyPrelude

newtype Colour = Colour { hex :: Text }

black :: Colour
black = Colour "#161618"

red :: Colour
red = Colour "#CD202D"

deathKnight :: Colour
deathKnight = Colour "#C41F3B"

demonHunter :: Colour
demonHunter = Colour "#A330C9"

druid :: Colour
druid = Colour "#FF7D0A"

hunter :: Colour
hunter = Colour "#A9D271"

mage :: Colour
mage = Colour "#40C7EB"

monk :: Colour
monk = Colour "#00FF96"

paladin :: Colour
paladin = Colour "#F58CBA"

priest :: Colour
priest = Colour "#FFFFFF"

rogue :: Colour
rogue = Colour "#FFF569"

shaman :: Colour
shaman = Colour "#0070DE"

warlock :: Colour
warlock = Colour "#8787ED"

warrior :: Colour
warrior = Colour "#C79C6E"