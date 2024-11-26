// GLOBALS
let battleState = null;
let enemies = null;
let friends = null;


/*************
 * FUNCTIONS *
 *************/

function getTeams(turnOrder) {
    enemies = [];
    friends = [];

    turnOrder.forEach(char => {
        if (char.team == "Friend") {
            friends.push(char);
        } else {
            enemies.push(char);
        }
    });
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
    portraitDiv.classList.add("portrait")
    const portraitSource = undefined;

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

    if (character.team == "Friend") {
        const friendDiv = document.querySelector(".friend-display");
        friendDiv.appendChild(characterDiv);
    } else {
        const enemyDiv = document.querySelector(".enemy-display");
        enemyDiv.appendChild(characterDiv);        
    }
}

function displayBoard(characters = null) {
    // Clear board
    const friendDiv = document.querySelector(".friend-display");
    friendDiv.innerHTML = "";
    const enemyDiv = document.querySelector(".enemy-display");
    enemyDiv.innerHTML = "";

    // Create each characters stat block and add to display
    characters.forEach(char => {
        createStatBlock(char);
    });
}


/*************
 * API CALLS *
 *************/
async function loadEncounter() {
    console.log("attempting to load encounter 1");
    const url = "http://localhost:3000/loadEncounter/1";

    try {
        const response = await fetch(url);
        if (response.status == 200) {
            battleState = await response.json();

            //get our response
            console.log("Fetch sucessful, returned JSON: ");
            console.log(battleState);
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
        testMessage2.textContent = "ERROR";
    }

    getTeams(battleState.turnOrder);
    displayBoard(battleState.turnOrder);
}

async function testAction() {

    let request = {
        action : ["SA","3","Rat"],
        turnOrder : (battleState.turnOrder)
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
            battleState = await response.json();
            console.log(battleState);
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
    }
}