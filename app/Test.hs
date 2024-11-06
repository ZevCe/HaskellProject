import Character
import Item

main = let 
        p1 = Character (Stamina 100) (Ki 50) (Speed 20)
        p2 = Character (Stamina 30) (Ki 50) (Speed 20)
            in
                do
                    print p1
                    print p2
                    print $ useHealthPotion p2
