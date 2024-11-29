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
let CurrentAttacker = null;

/*************
 * FUNCTIONS *
 *************/

function appendToCombatLog(toAppend) {
    const combatLogDiv = document.querySelector(".combat-log");
    const par = document.createElement("p");
    par.innerText = toAppend
    combatLogDiv.insertBefore(par, combatLogDiv.firstChild);
}

// function getTeams(turnOrder) {
//     enemies = [];
//     friends = [];

//     turnOrder.forEach(char => {
//         if (char.team == "Friend") {
//             friends.push(char);
//         } else {
//             enemies.push(char);
//         }
//     });
// }

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
    const portraitSource = "images/temp.png";
    portraitDiv.innerHTML = 
    `<img src=${portraitSource}></img>`;

    // Stat Block
    const statBlockDiv = document.createElement("div");
    statBlockDiv.classList.add("stat-block");
    statBlockDiv.innerHTML = 
    `<p class="name">${character.name}</p>
     <p class="stamina">${character.stamina} / ${character.maxStamina}</p>
     <p class="ki">${character.ki} / ${character.maxKi}</p>
     <p class="speed">${character.speed}</p>`;

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

function getNextTurn() {
    CurrentAttacker = BattleState.turnOrder[0];

    appendToCombatLog(`${CurrentAttacker.name} gets ready to attack!`);

    if (CurrentAttacker.team == "Friend") {
        appendToCombatLog("DEBUG: Friend");
        getMenu();
    } else {
        appendToCombatLog("DEBUG: Enemy");
    }
}

function getCharacters() {
    let classInfo = BattleState.classes;
    for (let i = 0; i < classInfo.length; i++) {
        let menu = [];
        let currentClass = classInfo[i];
        
        // There is definitely a better way to do this, but trying to figure
        // out a more elegant way wasn't working.
        // TODO: Refactor if time
        if (currentClass.amplify) {
            menu.push("amplify");
        }
        if (currentClass.barrier) {
            menu.push("barrier");
        }
        if (currentClass.curse) {
            menu.push("curse");
        }
        if (currentClass.dampen) {
            menu.push("dampen");
        }
        if (currentClass.demoralize) {
            menu.push("demoralize");
        }
        if (currentClass.heal) {
            menu.push("heal");
        }
        if (currentClass.intimidate) {
            menu.push("intimidate");
        }
        if (currentClass.invigorate) {
            menu.push("invigorate");
        }
        if (currentClass.kiAttack) {
            menu.push("kiAttack");
        }
        if (currentClass.rally) {
            menu.push("rally");
        }
        if (currentClass.shield) {
            menu.push("shield");
        }
        if (currentClass.stamAttack) {
            menu.push("stamAttack");
        }

        CharacterMenus.set(`${classInfo[i].id}`, menu);
    }
    // console.log(CharacterMenus);
}

function getSpellCost(move, type, level) {
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

function getEnemyTargets(move, type) {
    const targetDiv = document.querySelector(".target-menu");
    targetDiv.innerHTML = "";

    BattleState.turnOrder.forEach(char => {
        if (char.team == "Enemy") {
            const target = document.createElement("button");
            target.innerText = `${char.name}`;
            target.addEventListener("click", () => {
                appendToCombatLog(`DEBUG: Send level 1 ${type} ${move} to ${char.name}`);
            });
            targetDiv.appendChild(target);
        }
    });
}

function getBasicAttack(move, type) {
    let costType = move == "kiAttack" ? "ki" : "stamina";
    let cost1 = getSpellCost(move, type, 1);
    let cost2 = getSpellCost(move, type, 2);
    let cost3 = getSpellCost(move, type, 3);
    const attackMenuDiv = document.querySelector(".attack-menu");
    attackMenuDiv.innerHTML = "";

    if (type == "single") {
        // const submenuButtons = document.querySelectorAll(".sub-menu button");
        //     submenuButtons.forEach(button => {
        //         button.style.backgroundColor = "rgb(212, 180, 180)";
        // });

        const level0 = document.createElement("button");
        level0.innerText = `Level 0 (0)`;
        level0.addEventListener("click", () => {
            const attackButtons = document.querySelectorAll(".attack-menu button");
            attackButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            level0.style.backgroundColor = "green";
            getEnemyTargets(move, type);
        });
        attackMenuDiv.appendChild(level0);
        
        if (!isValidAttack(cost1, costType)) { return; }

        const level1 = document.createElement("button");
        level1.innerText = `Level 1 (${cost1})`;
        level1.addEventListener("click", () => {
            const attackButtons = document.querySelectorAll(".attack-menu button");
            attackButtons.forEach(button => {
                button.style.backgroundColor = "rgb(212, 180, 180)";
            });
            level1.style.backgroundColor = "green";
            getEnemyTargets(move, type);
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
            getEnemyTargets(move, type);
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
            getEnemyTargets(move, type);
        });
        attackMenuDiv.appendChild(level3);

        return;
    }

    if (!isValidAttack(cost1, costType)) { return; }

    const level1 = document.createElement("button");
    level1.innerText = `Level 1 (${cost1})`;
    level1.addEventListener("click", () => {
        appendToCombatLog(`DEBUG: Send level 1 ${type} ${move}`);
    });
    attackMenuDiv.appendChild(level1);

    if (!isValidAttack(cost2, costType)) { return; }

    const level2 = document.createElement("button");
    level2.innerText = `Level 2 (${cost2})`;
    level2.addEventListener("click", () => {
        appendToCombatLog(`DEBUG: Send level 1 ${type} ${move}`);
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
    if (move == "kiAttack" || move == "stamAttack") {
        let moveCode = move == "kiAttack" ? "KA" : "SA";
        getBasicAttack(moveCode , type);
        return;
    }

    // Ki attacks
    if (costType = "ki") {

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
    appendToCombatLog(`DEBUG: Printing out ${CurrentAttacker.name} menu`);

    const mainMenu = document.querySelector(".main-menu");
    mainMenu.innerHTML = "";
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
                appendToCombatLog(`DEBUG: Send ${move} to ${char.name}`);
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
                appendToCombatLog(`DEBUG: Send ${move} on ${char.name}`);
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
            getItemFriendlyTargets("MS");
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
    console.log(`Attempting to load encounter ${encounterName}`);
    appendToCombatLog(`Loading ${encounterName}`);
    const url = `http://localhost:3000/loadEncounter/${encounterName}`;

    try {
        const response = await fetch(url);
        if (response.status == 200) {
            BattleState = await response.json();

            //get our response
            console.log("Fetch sucessful, returned JSON: ");
            appendToCombatLog(`Encounter ${encounterName} loaded`);
            console.log(BattleState);
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
        testMessage2.textContent = "ERROR";
    }

    getCharacters();
    displayBoard();
    getTurnOrder();
    getNextTurn();
}

async function testAction() {

    let request = {
        action : ["SA","3","Rat"],
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
}