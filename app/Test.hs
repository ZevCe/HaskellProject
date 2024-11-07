import Character
import Item

main = let 
        p1 = Character (Stamina 100) (Ki 50) (Speed 20) Enemy
        p2 = Character (Stamina 30) (Ki 50) (Speed 20) Friend
            in
                do
                    print p1
                    print p2
                    print $ useHealthPotion p2

getTurnOrder :: [Character] -> [Character]
getTurnOrder characters = undefined
    -- where insert 