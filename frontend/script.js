// CONSTANTS
const STAM_MOVES = ["stamAttack", 
                    "rally", 
                    "invigorate", 
                    "demoralize", 
                    "intimidate", 
                    "shield"];

// GLOBALS
let BattleState = null;
let CharacterMenus = new Map();
let CharacterMenusBackend = new Map();
let CurrentAttacker = null;
let CurrentMove = null;

let Enemies = null;
let Friends = null;

/*************
 * FUNCTIONS *
 *************/

function appendToCombatLog(toAppend) {
    const combatLogDiv = document.querySelector(".combat-log");
    const par = document.createElement("p");
    par.innerText = toAppend
    combatLogDiv.insertBefore(par, combatLogDiv.firstChild);
}

function updateCombatLog() {
    let moves = BattleState.action;
    // console.log(moves);

    for (let i = 1; i < moves.length; i++) {
        appendToCombatLog(moves[i]);
    }

    appendToCombatLog("");
}

function getTeams() {
    turnOrder = BattleState.turnOrder;
    // console.log(turnOrder);

    Enemies = [];
    Friends = [];

    turnOrder.forEach(char => {
        if (char.team == "Friend") {
            Friends.push(char);
        } else {
            Enemies.push(char);
        }
    });

    // console.log(Enemies);
    // console.log(Friends);
}

function createStatBlock(character = null) {
    if (character == null) {
        console.log("Null character, error creating stat block");
        return;
    }

    // Character
    const characterDiv = document.createElement("div");
    characterDiv.classList.add("character");
    characterDiv.setAttribute("id", `${character.name}`);

    // TODO: Actually put a portrait in
    // Portrait
    const portraitDiv = document.createElement("div");
    portraitDiv.classList.add("portrait");
    let portraitSource = null;
    if (character.team == "Friend") {
        portraitSource = "images/temp-friend.png";
    } else {
        portraitSource = "images/temp.png";
    }
    portraitDiv.innerHTML = 
    `<img src=${portraitSource}></img>`;

    // Stat Block
    const statBlockDiv = document.createElement("div");
    statBlockDiv.classList.add("stat-block");
    statBlockDiv.innerHTML = 
    `<p class="name">${character.name}</p>
     <p class="stamina">${character.stamina} / ${character.maxStamina}</p>
     <p class="ki">${character.ki} / ${character.maxKi}</p>
     <p class="speed">${character.speed}</p>
     <p class="status">Statuses go here</p>`;

    // Combining div
    characterDiv.appendChild(portraitDiv);
    characterDiv.appendChild(statBlockDiv);

    // Place on correct side
    if (character.team == "Friend") {
        const friendDiv = document.querySelector(".friend-display");
        friendDiv.appendChild(characterDiv);
    } else {
        const enemyDiv = document.querySelector(".enemy-display");
        enemyDiv.appendChild(characterDiv);        
    }

    setStatusDiv(character);
}

function setStatusDiv(character) {
    const statuses = document.querySelector(`#${character.name} .stat-block .status`);
    // Statues: Red debuff, green buff
    statuses.innerText = "";
    if (character.statuses.length == 0) {
        // Replace with "" after testing
    } else {
        // Go through the list and add each buff/debuff
        statuses.innerText = createStatusString(character.name, character.statuses);
    }
}

function createStatusString(name, statuses) {
    returnString = "";
    
    statuses.forEach(status => {
        switch (status.trim()) {
            case "webTrap":
                returnString += "W";
                break;
            case "hastePotion":
                returnString += "H";
                break;
            case "invigorate":
                returnString += "I";
                break;
            case "demoralize":
                returnString += "D";
                break;
            case "intimidate":
                returnString += "I";
                break;
            case "shield":
                returnString += "S";
                break;
            case "amplify":
                returnString += "A";
                break;
            case "dampen":
                returnString += "D";
                break;
            case "curse":
                returnString += "C";
                break;
            case "barrier":
                returnString += "B";
                break;
        }
    });
    return returnString;
}

function displayBoard() {
    // Clear board
    const friendDiv = document.querySelector(".friend-display");
    friendDiv.innerHTML = "";
    const enemyDiv = document.querySelector(".enemy-display");
    enemyDiv.innerHTML = "";

    // Create each characters stat block and add to display
    let characters = BattleState.turnOrder;
    characters.forEach(char => {
        createStatBlock(char);
    });
}

function endGame() {
    const mainMenu = document.querySelector(".main-menu");
    mainMenu.innerHTML = "";
    const subMenu = document.querySelector(".sub-menu");
    subMenu.innerHTML = "";
    const attackMenu = document.querySelector(".attack-menu");
    attackMenu.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    if (Enemies.length == 0) {
        const enemyDiv = document.querySelector(".enemy-display");
        enemyDiv.innerHTML = "<p>You Win!</p>";
    } else {
        const friendDiv = document.querySelector(".friend-display");
        friendDiv.innerHTML = "<p>You lose!</p>";
    }
}

function getNextTurn() {
    CurrentAttacker = BattleState.turnOrder[0];
    // console.log(CurrentAttacker);

    getTeams();

    if (Friends.length == 0 || Enemies.length == 0) {
        endGame();
        return;
    }

    appendToCombatLog(`${CurrentAttacker.name} is ready to attack!`);

    if (CurrentAttacker.team == "Friend") {
        // appendToCombatLog("DEBUG: Friend");
        getMenu();
    } else {
        // appendToCombatLog("DEBUG: Enemy");
        // action("Ea", null, null);
        getMenu();
    }
}

function getCharacters() {
    let classInfo = BattleState.classes;
    for (let i = 0; i < classInfo.length; i++) {
        let menu = [];
        let menuBackend = [];
        let currentClass = classInfo[i];
        
        // There is definitely a better way to do this, but trying to figure
        // out a more elegant way wasn't working.
        // TODO: Refactor if time
        if (currentClass.amplify) {
            menu.push("amplify");
            menuBackend.push("Amp");
        }
        if (currentClass.barrier) {
            menu.push("barrier");
            menuBackend.push("Brr");
        }
        if (currentClass.curse) {
            menu.push("curse");
            menuBackend.push("Crs");
        }
        if (currentClass.dampen) {
            menu.push("dampen");
            menuBackend.push("Damp");
        }
        if (currentClass.demoralize) {
            menu.push("demoralize");
            menuBackend.push("Demor");
        }
        if (currentClass.heal) {
            menu.push("heal");
            menuBackend.push("Hl");
        }
        if (currentClass.intimidate) {
            menu.push("intimidate");
            menuBackend.push("Intim");
        }
        if (currentClass.invigorate) {
            menu.push("invigorate");
            menuBackend.push("Invig");
        }
        if (currentClass.kiAttack) {
            menu.push("kiAttack");
            menuBackend.push("KA");
        }
        if (currentClass.rally) {
            menu.push("rally");
            menuBackend.push("Rly");
        }
        if (currentClass.shield) {
            menu.push("shield");
            menuBackend.push("Shld");
        }
        if (currentClass.stamAttack) {
            menu.push("stamAttack");
            menuBackend.push("SA");
        }

        CharacterMenus.set(`${classInfo[i].id}`, menu);
        CharacterMenusBackend.set(`${classInfo[i].id}`, menuBackend);
    }
    // console.log(CharacterMenus);
    // console.log(CharacterMenusBackend);
}

function getSpellCost(type, level) {
    if (type == "single") {
        return Math.abs(Math.min((5 + (-15) * level), 0));
    } else {
        return 30 * level;
    }
}

function isValidAttack(cost, costType) {
    let attackerResources = 
        costType == "ki" ? CurrentAttacker.ki : CurrentAttacker.stamina;
    
    if (attackerResources > cost) {
        return true;
    }

    return false;
}

function getFriendlyTargets(move, type, level) {
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    BattleState.turnOrder.forEach(char => {
        if (char.team == "Friend") {
            const target = document.createElement("button");
            target.innerText = `${char.name}`;
            target.addEventListener("click", () => {
                // appendToCombatLog(`DEBUG: Send level ${level} ${type} ${move} to ${char.name}`);
                action(move, level, target.innerText);
            });
            targetDiv.appendChild(target);
        }
    });
}

function getEnemyTargets(move, type, level) {
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    BattleState.turnOrder.forEach(char => {
        if (char.team == "Enemy") {
            const target = document.createElement("button");
            target.innerText = `${char.name}`;
            target.addEventListener("click", () => {
                // appendToCombatLog(`DEBUG: Send level ${level} ${type} ${move} to ${char.name}`);
                action(move, level, target.innerText);
            });
            targetDiv.appendChild(target);
        }
    });
}

function getStatus(move, type, costType) {
    let costSingle = 25;
    let costGroup = 60;
    const attackMenuDiv = document.querySelector(".attack-menu");
    attackMenuDiv.innerHTML = "";

    if (type == "single") {
        if (move == "Invig" || move == "Shld" || move == "Hl" || move == "Brr") {
            const targetDiv = document.querySelector(".attack-menu");
            targetDiv.innerHTML = "";
        
            BattleState.turnOrder.forEach(char => {
                if (char.team == "Friend") {
                    const target = document.createElement("button");
                    target.innerText = `${char.name} (${costSingle})`;
                    target.addEventListener("click", () => {
                        // appendToCombatLog(`DEBUG: Send level ${level} ${type} ${move} to ${char.name}`);
                        action(move, 0, char.name);
                    });
                    targetDiv.appendChild(target);
                }
            });
        } else {
            const targetDiv = document.querySelector(".attack-menu");
            targetDiv.innerHTML = "";
        
            BattleState.turnOrder.forEach(char => {
                if (char.team == "Enemy") {
                    const target = document.createElement("button");
                    target.innerText = `${char.name} (${costSingle})`;
                    target.addEventListener("click", () => {
                        // appendToCombatLog(`DEBUG: Send level ${level} ${type} ${move} to ${char.name}`);
                        action(move, 0, char.name);
                    });
                    targetDiv.appendChild(target);
                }
            });
        }

        return;
    }

    if (!isValidAttack(costGroup, costType)) { return; }

    const level2 = document.createElement("button");
    level2.innerText = `Cast ${move} (${costGroup})`;
    level2.addEventListener("click", () => {
        // appendToCombatLog(`DEBUG: Send level 2 ${type} ${move}`);
        action(move, 2, "A");
    });
    attackMenuDiv.appendChild(level2);
}

function getBasicAttack(move, type, costType) {
    let cost1 = getSpellCost(type, 1);
    let cost2 = getSpellCost(type, 2);
    let cost3 = getSpellCost(type, 3);
    const attackMenuDiv = document.querySelector(".attack-menu");
    attackMenuDiv.innerHTML = "";

    if (move == "Invig" || move == "Demor" || move == "Intim" || move == "Shld" ||
        move == "Amp" || move == "Damp" || move == "Crs" || move == "Brr") {
            getStatus(move, type, costType);
            return;
        }

    if (type == "single") {
        // const submenuButtons = document.querySelectorAll(".sub-menu button");
        //     submenuButtons.forEach(button => {
        //         button.style.backgroundColor = "rgb(212, 180, 180)";
        // });

        // Special case for 0 cost attacks for kiAttack/stamAttack
        if (move == "KA" || move == "SA") {
            const level0 = document.createElement("button");
            level0.innerText = `Level 0 (0)`;
            level0.addEventListener("click", () => {
                const attackButtons = document.querySelectorAll(".attack-menu button");
                attackButtons.forEach(button => {
                    button.style.backgroundColor = "rgb(212, 180, 180)";
                });
                level0.style.backgroundColor = "green";
                getEnemyTargets(move, type, 0);
            });
            attackMenuDiv.appendChild(level0);
        }
        
        if (!isValidAttack(cost1, costType)) { return; }

        const level1 = document.createElement("button");
        level1.innerText = `Level 1 (${cost1})`;
        level1.addEventListener("click", () => {
            const attackButtons = document.querySelectorAll(".attack-menu button");
            attackButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            level1.style.backgroundColor = "green";
            if (move == "Invig" || move == "Shld" || move == "Hl" || move == "Brr") {
                getFriendlyTargets(move, type, 1);
            } else {
                getEnemyTargets(move, type, 1);
            }
        });
        attackMenuDiv.appendChild(level1);

        if (!isValidAttack(cost2, costType)) { return; }

        const level2 = document.createElement("button");
        level2.innerText = `Level 2 (${cost2})`;
        level2.addEventListener("click", () => {
            const attackButtons = document.querySelectorAll(".attack-menu button");
            attackButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            level2.style.backgroundColor = "green";
            if (move == "Invig" || move == "Shld" || move == "Hl" || move == "Brr") {
                getFriendlyTargets(move, type, 2);
            } else {
                getEnemyTargets(move, type, 2);
            }
        });
        attackMenuDiv.appendChild(level2);

        if (!isValidAttack(cost3, costType)) { return; }

        const level3 = document.createElement("button");
        level3.innerText = `Level 3 (${cost3})`;
        level3.addEventListener("click", () => {
            const attackButtons = document.querySelectorAll(".attack-menu button");
            attackButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            level3.style.backgroundColor = "green";
            if (move == "Invig" || move == "Shld" || move == "Hl" || move == "Brr") {
                getFriendlyTargets(move, type, 3);
            } else {
                getEnemyTargets(move, type, 3);
            }
        });
        attackMenuDiv.appendChild(level3);

        return;
    }

    if (!isValidAttack(cost1, costType)) { return; }

    const level1 = document.createElement("button");
    level1.innerText = `Level 1 (${cost1})`;
    level1.addEventListener("click", () => {
        // appendToCombatLog(`DEBUG: Send level 1 ${type} ${move}`);
        action(move, 1, "A");
    });
    attackMenuDiv.appendChild(level1);

    if (!isValidAttack(cost2, costType)) { return; }

    const level2 = document.createElement("button");
    level2.innerText = `Level 2 (${cost2})`;
    level2.addEventListener("click", () => {
        // appendToCombatLog(`DEBUG: Send level 2 ${type} ${move}`);
        action(move, 2, "A");
    });
    attackMenuDiv.appendChild(level2);
}

function getSpells(move, type) {
    const attackMenu = document.querySelector(".attack-menu");
    attackMenu.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    let costType = "ki";
    if (STAM_MOVES.includes(move)) {
        costType = "stamina";
    }

    // Basic attacks
    // if (move == "kiAttack" || move == "stamAttack") {
    //     let moveCode = move == "kiAttack" ? "KA" : "SA";
    //     getBasicAttack(moveCode , type);
    //     return;
    // }

    switch (move) {
        case "kiAttack":
            getBasicAttack("KA", type, costType);
            break;
        case "stamAttack":
            getBasicAttack("SA", type, costType);
            break;
        case "rally":
            getBasicAttack("Rly", type, costType);
            break;
        case "invigorate":
            getBasicAttack("Invig", type, costType);
            break;
        case "demoralize":
            getBasicAttack("Demor", type, costType);
            break;
        case "intimidate":
            getBasicAttack("Intim", type, costType);
            break;
        case "shield":
            getBasicAttack("Shld", type, costType);
            break;
        case "heal":
            getBasicAttack("Hl", type, costType);
            break;
        case "amplify":
            getBasicAttack("Amp", type, costType);
            break;
        case "barrier":
            getBasicAttack("Brr", type, costType);
            break;
        case "dampen":
            getBasicAttack("Damp", type, costType);
            break;
        case "curse":
            getBasicAttack("Crs", type, costType);
            break;
        default:
            appendToCombatLog("Invalid move (Should not reach here)");
            break;
    }
}

function getSingleOrGroup(move) {
    const subMenu = document.querySelector(".sub-menu");
    subMenu.innerHTML = "";
    const attackMenu = document.querySelector(".attack-menu");
    attackMenu.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    // Single target
    const single = document.createElement("button");
    single.innerText = `Single`;
    single.addEventListener("click", () => {
        const submenuButtons = document.querySelectorAll(".sub-menu button");
        submenuButtons.forEach(button => {
            button.style.backgroundColor = "rgb(212, 180, 180)";
        });
        single.style.backgroundColor = "green";
        getSpells(move, "single");
    });
    subMenu.appendChild(single);

    // Group attack
    const group = document.createElement("button");
    group.innerText = `Group`;
    group.addEventListener("click", () => {
        const submenuButtons = document.querySelectorAll(".sub-menu button");
        submenuButtons.forEach(button => {
            button.style.backgroundColor = "rgb(212, 180, 180)";
        });
        group.style.backgroundColor = "green";
        getSpells(move, "group");
    });
    subMenu.appendChild(group);
}

function getMenu() {
    // appendToCombatLog(`DEBUG: Printing out ${CurrentAttacker.name} menu`);
    const mainMenu = document.querySelector(".main-menu");
    mainMenu.innerHTML = "";
    const subMenu = document.querySelector(".sub-menu");
    subMenu.innerHTML = "";
    const attackMenu = document.querySelector(".attack-menu");
    attackMenu.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    let moves = CharacterMenus.get(CurrentAttacker.name);

    moves.forEach(move => {
        let moveButton = document.createElement("button");
        moveButton.classList.add(move);
        // TODO: Change to make more readable
        // TODO: Make sta/ki attack first in menu
        moveButton.innerText = move;
        moveButton.addEventListener("click", () => {
            const mainMenuButtons = document.querySelectorAll(".main-menu button");
            mainMenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            moveButton.style.backgroundColor = "green";
            getSingleOrGroup(move);
        })
        mainMenu.appendChild(moveButton);
    });

    // Items
    const items = document.createElement("button");
    items.classList.add("item");
    items.innerText = "Items";
    items.addEventListener("click", () => {
        const mainMenuButtons = document.querySelectorAll(".main-menu button");
        mainMenuButtons.forEach(button => {
            button.style.backgroundColor = "rgb(212, 180, 180)";
        });
        items.style.backgroundColor = "green";
        getItems();
    });
    mainMenu.appendChild(items);
}

function getItemEnemyTargets(move) {
    const attackDiv = document.querySelector(".attack-menu");
    attackDiv.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    BattleState.turnOrder.forEach(char => {
        if (char.team == "Enemy") {
            const target = document.createElement("button");
            target.innerText = `${char.name}`;
            target.addEventListener("click", () => {
                // appendToCombatLog(`DEBUG: Send ${move} to ${char.name}`);
                action(move, null, char.name);
            });
            attackDiv.appendChild(target);
        }
    });
}

function getItemFriendlyTargets(move) {
    const attackDiv = document.querySelector(".attack-menu");
    attackDiv.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    BattleState.turnOrder.forEach(char => {
        if (char.team == "Friend") {
            const target = document.createElement("button");
            target.innerText = `${char.name}`;
            target.addEventListener("click", () => {
                // appendToCombatLog(`DEBUG: Send ${move} on ${char.name}`);
                action(move, null, char.name);
            });
            attackDiv.appendChild(target);
        }
    });
}

function getItems() {
    // appendToCombatLog(`DEBUG: Printing out ${CurrentAttacker.name} items`);

    let items = BattleState.turnOrder[0].items;

    const subMenu = document.querySelector(".sub-menu");
    subMenu.innerHTML = "";
    const attackMenu = document.querySelector(".attack-menu");
    attackMenu.innerHTML = "";
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    if (items.numHastePotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("haste-potion");
        potion.innerText = `Haste Potion (${items.numHastePotions})`;
        potion.addEventListener("click", () => {
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
            getItemFriendlyTargets("HaP");
        });
        subMenu.appendChild(potion);
    }

    if (items.numHealthPotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("health-potion");
        potion.innerText = `Health Potion (${items.numHealthPotions})`;
        potion.addEventListener("click", () => {
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
            getItemFriendlyTargets("HeP");
        });
        subMenu.appendChild(potion);
    }

    if (items.numMagicalSeals != 0) {
        const potion = document.createElement("button");
        potion.classList.add("magical-seal");
        potion.innerText = `Magical Seal (${items.numMagicalSeals})`;
        potion.addEventListener("click", () => {
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
            getItemEnemyTargets("MS");
        });
        subMenu.appendChild(potion);
    }

    if (items.numManaPotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("mana-potion");
        potion.innerText = `Mana Potion (${items.numManaPotions})`;
        potion.addEventListener("click", () => {
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
            getItemFriendlyTargets("MP");
        });
        subMenu.appendChild(potion);
    }

    if (items.numThrowingKnives != 0) {
        const potion = document.createElement("button");
        potion.classList.add("throwing-knife");
        potion.innerText = `Throwing Knife (${items.numThrowingKnives})`;
        potion.addEventListener("click", () => {
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
            getItemEnemyTargets("TK");
        });
        subMenu.appendChild(potion);
    }

    if (items.numWebTraps != 0) {
        const potion = document.createElement("button");
        potion.classList.add("web-trap");
        potion.innerText = `Web Trap (${items.numWebTraps})`;
        potion.addEventListener("click", () => {
            getItemEnemyTargets("WT");
            const submenuButtons = document.querySelectorAll(".sub-menu button");
            submenuButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            potion.style.backgroundColor = "green";
        });
        subMenu.appendChild(potion);
    }
}

function getTurnOrder() {
    const turnOrderDiv = document.querySelector("#turn-order");
    turnOrderDiv.innerHTML = `<p>Turn Order</p>`;

    let turnOrder = "";
    BattleState.turnOrder.forEach(char => {
        turnOrderDiv.innerHTML += `<p>${char.name}</p>`;
    });
}

/*************
 * API CALLS *
 *************/
async function loadEncounter(encounterName) {
    // console.log(`Attempting to load encounter ${encounterName}`);
    // appendToCombatLog(`Loading ${encounterName}`);
    const url = `http://localhost:3000/loadEncounter/${encounterName}`;

    try {
        const response = await fetch(url);
        if (response.status == 200) {
            BattleState = await response.json();

            //get our response
            console.log("Fetch sucessful, returned JSON: ");
            // appendToCombatLog(`Encounter ${encounterName} loaded`);
            console.log(BattleState);
            const cl = document.querySelector(".combat-log");
            cl.innerHTML = "";
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
        testMessage2.textContent = "ERROR";
    }

    //renaming our json to match the turn json packet better
    let classesTemp = BattleState.classes
    let turnOrderTemp = BattleState.turnOrderInit

    BattleState = {
        classes : classesTemp,
        turnOrder : turnOrderTemp
    }

    getCharacters(); // Run at beginning
    displayBoard();
    getTurnOrder();
    getNextTurn();
}

async function action(move, level, target) {
    let a = null;

    if (move == "Ea") { // Enemy attack
        appendToCombatLog(`${CurrentAttacker.name} attacks`);
        const temp = ["Ea"];
        a = temp.concat(CharacterMenusBackend.get(CurrentAttacker.name));
    } else if (level == null) { // Item use
        appendToCombatLog(`${CurrentAttacker.name} uses ${move}`);
        a = [move, target];
    } else if (move == "Invig" || move == "Demor" || move == "Intim" || move == "Shld" 
        || move == "Amp" || move == "Damp" || move == "Crs" || move == "Brr") {
        appendToCombatLog(`${CurrentAttacker.name} attacks`);
        a = [move, target];
    } else {
        appendToCombatLog(`${CurrentAttacker.name} attacks`);
        a = [move, level.toString(), target];
    }

    console.log(a);

    // action : ["SA","3","Rat"],

    let request = {
        action : a,
        turnOrder : (BattleState.turnOrder)
    }

    console.log("Sending JSON: ");
    console.log(request);

    const exportUrl = "http://localhost:3000/roundInfo";

    try {
        const response = await fetch(exportUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(request)
        });
        if (response.status == 200) {

            //get our response
            console.log("receiving JSON: ");
            BattleState = await response.json();
            console.log(BattleState);
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
    }

    updateCombatLog();
    displayBoard(); // TODO: Create update board to give delays and such
    getTurnOrder();
    getNextTurn();
}

const tutorial = document.querySelector("#tut");
tutorial.addEventListener("click", () => {

    loadEncounter(-1);
});

const e1 = document.querySelector("#e1");
e1.addEventListener("click", () => {
    loadEncounter(1);
});

const e2 = document.querySelector("#e2");
e2.addEventListener("click", () => {
    loadEncounter(2);
});