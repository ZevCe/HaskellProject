module ModifyStats where
import Character

makeCharacter sta k spd t = 
    Character sta sta k k spd [] t

modifyStamina char mod = 
    Character (stamina char + mod) (maxStamina char) (ki char) (maxKi char) (speed char) (statuses char) (team char)

modifyKi char mod =  
    Character (stamina char) (maxStamina char) (ki char + mod) (maxKi char) (speed char) (statuses char) (team char)

modifySpeed char mod = 
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char + mod) (statuses char) (team char)

modifyStatuses char newStatuses = 
    Character (stamina char) (maxStamina char) (ki char) (maxKi char) (speed char) (newStatuses) (team char)

