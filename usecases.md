What we need to figure out before we can start programming
1) What stats every character will have (strength, dex, health, mana, etc.)
2) What actions every character can take (attack, guard, cast spell, flee, etc.)
3) How the stats of the characters will influence the chances of sucess of the attempted actions and when those actions will occur

# Stat block:
* speed: 0-100 (fastest boys move first, ties broken by hashing), how early in the round you get to move
* stamina 0-100 (phys health/resource/atk/defense)
* ki 0-100 (magic health/resource/atk/defense)
* when stamina OR ki reaches 0, character is knocked out

# Actions (ordered in priority of implementation): 
1) basic attack (is free, uses ki/stamina based on class)
2) skills (costs resource based on class)
    1) damaging skills 
    2) class skill
3) basic items (healing and combat)

# Damage Formulas:
1) Basic attack:
    * atk = max stamina or ki of attacker
    * def = max stamina or ki of defender
    * note that if attacker is using stamina then defender must also be using and vice versa
    * atk/def * 10
2) skills
    *  damaging skills:
        * exist in power brackets of [10, 25, 40] in terms of resource consumption
        * 10 = 1.2x multiplier to basic attack formula
        * 25 = 1.6x multiplier to basic attack formula
        * 40 = 2x multiplier to basic attack formula
    * healing = undefined
    * buff, your next attack will do 1.5x damage
    * debuff, a selected enemy will take 1.5x damage
    * shield, the enemy's next physical attack must target you or another shielding ally, will do .2x damage
    * barrier, the enemy's next magical attack must target you or another barriering ally, will do .2x damage
    * 
    * 
3) Items
    * health potions, restores set amt of stamina
    * mana potion, restores set amt of ki
    * throwing knives, does set amt of stamina damage
    * magical seal, does set amt of ki damage
    * web trap, halves an opponents speed
    * haste potion, doubles speed for rest of battle