async function test() {
    let json = {"name":"James", "age":25}

            console.log(JSON.stringify(json));

            const exportUrl = "http://localhost:3000/api/user";

            try {
                const response = await fetch(exportUrl, {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(json)
                });
                if (response.status == 200) {

                    //get our response
                    const obj = await response.json();
                    console.log("Success" + JSON.stringify(obj));
                }
                else {
                    console.log("response.status error: " + response.status);
                }
            }
            catch (error) {
                console.error("ERROR! ", error);
            }
}