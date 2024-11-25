async function test() {

    //In all cases going forward you will make a fetch request to /loadEncounter to generate the initial list of characters
    //this is purely for testing this one time, because I couldn't be bothered to take the 30 seconds needed to refactor encounter
    let sampleRequest = {
        action : ["HeP","james"],
        turnOrder : [
            {
                name : "zev",
                team : "Friend",
                items : {numHealthPotions : 1, numManaPotions : 0, numThrowingKnives : 0, numMagicalSeals : 0, numWebTraps : 0, numHastePotions : 0},
                stamina : 1000000,
                maxStamina : 0,
                ki : 100000,
                maxKi : 0,
                speed : 100000,
                statuses : []
            },
            {
                name : "james",
                team : "Friend",
                items : {numHealthPotions : 0, numManaPotions : 0, numThrowingKnives : 0, numMagicalSeals : 0, numWebTraps : 0, numHastePotions : 0},
                stamina : 5,
                maxStamina : 25,
                ki : 1,
                maxKi : 1,
                speed : 0,
                statuses : []
            }
        ]
    }

    console.log("Sending JSON: ");
    console.log(sampleRequest);

    const exportUrl = "http://localhost:3000/roundInfo";

    try {
        const response = await fetch(exportUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(sampleRequest)
        });
        if (response.status == 200) {

            //get our response
            console.log("receiving JSON: ");
            const obj = await response.json();
            console.log(obj);
        }
        else {
            console.log("response.status error: " + response.status);
        }
    }
    catch (error) {
        console.error("ERROR! ", error);
    }
}