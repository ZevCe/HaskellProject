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

function getMoves(move) {

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
            getMoves(move);
        })
        mainMenu.appendChild(moveButton);
    });

    // Items
    const items = document.createElement("button");
    items.classList.add("item");
    items.innerText = "Items";
    items.addEventListener("click", () => {
        getItems();
    });
    mainMenu.appendChild(items);
}

function getItems() {
    appendToCombatLog(`DEBUG: Printing out ${CurrentAttacker.name} items`);

    let items = BattleState.turnOrder[0].items;

    if (items.length == 0) {
        return;
    }

    const subMenu = document.querySelector(".sub-menu");
    subMenu.innerHTML = "";

    if (items.numHastePotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("haste-potion");
        potion.innerText = `Haste Potion (${items.numHastePotions})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Haste Potion");
        });
        subMenu.appendChild(potion);
    }

    if (items.numHealthPotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("health-potion");
        potion.innerText = `Health Potion (${items.numHealthPotions})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Health Potion");
        });
        subMenu.appendChild(potion);
    }

    if (items.numMagicalSeals != 0) {
        const potion = document.createElement("button");
        potion.classList.add("magical-seal");
        potion.innerText = `Magical Seal (${items.numMagicalSeals})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Magical Seal");
        });
        subMenu.appendChild(potion);
    }

    if (items.numManaPotions != 0) {
        const potion = document.createElement("button");
        potion.classList.add("mana-potion");
        potion.innerText = `Mana Potion (${items.numManaPotions})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Mana Potion");
        });
        subMenu.appendChild(potion);
    }

    if (items.numThrowingKnives != 0) {
        const potion = document.createElement("button");
        potion.classList.add("throwing-knife");
        potion.innerText = `Throwing Knife (${items.numThrowingKnives})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Throwing Knife");
        });
        subMenu.appendChild(potion);
    }

    if (items.numWebTraps != 0) {
        const potion = document.createElement("button");
        potion.classList.add("web-trap");
        potion.innerText = `Web Trap (${items.numWebTraps})`;
        potion.addEventListener("click", () => {
            appendToCombatLog("Use Web Trap");
        });
        subMenu.appendChild(potion);
    }
}

/*************
 * API CALLS *
 *************/
async function loadEncounter(encounterName) {
    console.log(`Attempting to load encounter ${encounterName}`);
    appendToCombatLog(`Loading ${encounterName}`);
    const url = "http://localhost:3000/loadEncounter/1";

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