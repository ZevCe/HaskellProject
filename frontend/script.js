let battleState = null;

async function loadEncounter() {
    console.log("attempting to load encounter 1");
    const url = "http://localhost:3000/loadEncounter/1";

    try {
        const response = await fetch(url);
        if (response.status == 200) {
            battleState = await response.json();

            //get our response
            console.log("Fetch sucessful, returned JSON: ");
            console.log(battleState)
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
        testMessage2.textContent = "ERROR";
    }
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