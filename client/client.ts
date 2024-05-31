import assert from "assert";
import puppeteer from "puppeteer";
import { Page } from "puppeteer";
import swipl from "swipl";
import { term } from "swipl";
import { WORDS_TARGET } from "./wordlist";
const { list, compound, variable, serialize } = term;

const CHANGELOG_CLOSE_SELECTOR = "._modal_y9oz3_1 > ._button_1xh0d_1";
const PRACTICE_TAB_SELECTOR = "div._tabs_5us1c_1 > div:nth-child(2) > button";
const PRACTICE_LINK_SELECTOR =
  "div._tabWrapper_ag3fe_24 > div._tabContainer_ag3fe_30._shown_ag3fe_36 > div:nth-child(1) > button";
const MAIN_SELECTOR = "._main_kv0wd_1";
const BOARDS_SELECTOR = "div._board_1277w_1";
const CELL_SELECTOR = "div._cell_1277w_56";

const KEEP_PLAYING_SELECTOR = "#root > div > div._modalWrapper_y9oz3_1._lightweight_y9oz3_37 > div._modal_y9oz3_1 > div > button:nth-child(1)"
const STARTER_WORDS = ["TARES"];

async function gather_results(page: Page): Promise<string[]> {
  const boards = await page.$$(BOARDS_SELECTOR);
  const resultsPromises = boards.map(async (board) => {
    const boardCls = await board.evaluate((el) => el.className);
    if (boardCls.includes("dimmed")) {
      return "fffff";
    }

    const cells = await board.$$eval(CELL_SELECTOR, (els) =>
      els.map((el) => el.className),
    );
    const result = cells.slice(-10, -5);
    return result
      .map((cls) =>
        cls.includes("green") ? "f" : cls.includes("yellow") ? "p" : "i",
      )
      .join("");
  });

  const results = await Promise.all(resultsPromises);
  assert(
    results.length === 32,
    `Expected 32 boards, but found ${results.length}`
  );
  console.log("All board results:", results);
  return results;
}

function solvedAmount(board: string[]): number {
  const solve = board[board.length - 1];
  return (
    (solve.match(/f/g) ?? []).length + 0.5 * (solve.match(/p/g) ?? []).length
  );
}

function possibleSolutions(board, words) {
  return words.filter((word) => {
    return board.every((result) => {
      return isValidSolution(word, result);
    });
  });
}

function isValidSolution(word, result) {
  for (let i = 0; i < word.length; i++) {
    if (
      (result[i] === "f" && word[i] !== result[i]) ||
      (result[i] === "p" &&
        word.includes(result[i]) &&
        word[i] !== result[i]) ||
      (result[i] === "i" && word.includes(result[i]))
    ) {
      return false;
    }
  }
  return true;
}

(async () => {
  // Launch the browser and open a new blank page
  const browser = await puppeteer.launch({ headless: false });
  const page = await browser.newPage();

  // Navigate the page to a URL
  await page.goto("https://duotrigordle.com/game/practice/normal");

  await page.waitForSelector(CHANGELOG_CLOSE_SELECTOR);
  await page.click(CHANGELOG_CLOSE_SELECTOR);
  await page.click(PRACTICE_TAB_SELECTOR);

  await page.waitForSelector(PRACTICE_LINK_SELECTOR);
  // for whatever reason the events take a sec to get hooked up
  await new Promise((resolve) => setTimeout(resolve, 150));
  await page.click(PRACTICE_LINK_SELECTOR);

  const guesses: string[] = [];
  const all_results: string[][] = Array(32)
    .fill(null)
    .map((_) => []);
  for (const starter of STARTER_WORDS) {
    await page.type(MAIN_SELECTOR, starter);
    await page.keyboard.press("Enter");

    guesses.push(starter);
    const results = await gather_results(page);
    all_results.forEach((acc, i) => acc.push(results[i]));
  }

  swipl.call("['../entropy/entropy']");

  while (true) {
    page.click(KEEP_PLAYING_SELECTOR);
    
    const first_board = all_results.reduce((best_board, board) => {
      const solvedAmountBoard = solvedAmount(board);
      const possibleSolutionsBoard = possibleSolutions(
        board,
        WORDS_TARGET
      ).length;

      const solvedAmountBestBoard = solvedAmount(best_board);
      const possibleSolutionsBestBoard = possibleSolutions(
        best_board,
        WORDS_TARGET
      ).length;

      if (
        solvedAmountBoard >= solvedAmountBestBoard &&
        possibleSolutionsBoard <= possibleSolutionsBestBoard &&
        solvedAmountBoard !== 5
      ) {
        return board;
      }
      return best_board;
    }, all_results[0]);

    console.log("selected board:", first_board.at(-1));

    const query = serialize(
      compound("max_entropies_given", [
        list(guesses),
        list(first_board),
        variable("ME"),
      ])
    );
    console.log("querying prolog:", query);
    const ret = swipl.call(query);
    console.log("result:", ret.ME);

    if (!ret) {
      console.error("prolog query failed!");
      break;
    }

    await page.type(MAIN_SELECTOR, ret.ME);
    await page.keyboard.press("Enter");

    // await new Promise((resolve) => setTimeout(resolve, 1000));

    guesses.push(ret.ME);

    const results = await gather_results(page);
    all_results.map((acc, i) => acc.push(results[i]));
  }

  //await browser.close();
})();
