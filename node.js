const http = require('http');
const axios = require('axios');
const fs = require('fs');
const map = {};
const threads = 50;

function parseFile(filename) {
    return new Promise((resolve) => {
        fs.readFile(filename, async (err, data) => {
            const lines = data.toString().split('\n');
            for (let i = 0; i < lines.length; i += threads) {
                const ary = [];
                for (let j = 0; j < threads; ++j) {
                    const line = lines[i + j];
                    if (!line) {
                        continue;
                    }
                    const word = line.split(' ')[0].match( /[a-z]*/)[0];
                    if (word && !map[word]) {
                        ary.push(
                            axios.get(`http://apis.dict.cn/ajax/suggestion.php?callback=sugg1&q=${word}&dict=juhai&s=juhai&lt=`)
                                .then((res) => {
                                    const data = JSON.parse(res.data.match(/{.*}/)[0]).s[0];
                                    if (data) {
                                        map[word] = `${word} | ${data.g} | ${data.e}`
                                        console.log(word);
                                    } else {
                                        console.log(word, '没找到例句');
                                    }
                                })
                        );
                    }
                }
                await Promise.all(ary);
            }
            resolve();
        })
    })
}

async function main() {
    await parseFile("./CET4_edited.txt")
    await parseFile("./CET6_edited.txt");
    await parseFile("./TOEFL.txt");
    fs.writeFileSync("./paragraph.txt", Object.values(map).join('\n'));
}

main();
