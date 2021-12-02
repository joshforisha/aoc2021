const decoder = new TextDecoder("utf-8");
const depthData = await Deno.readFile("./01/depths.txt");

const depths = decoder.decode(depthData).split("\n").map((s) => parseInt(s));

// Part One
const [increases] = depths.slice(1).reduce(
  ([inc, y], x) => [inc + (x > y ? 1 : 0), x],
  [0, depths[0]],
);
console.log(`Increases: ${increases}`);

// Part Two
const [windowedIncreases] = depths.slice(1, -3).reduce(
  ([inc, y], x, i) => {
    const sum = x + depths[i + 2] + depths[i + 3];
    return [inc + (sum > y ? 1 : 0), sum];
  },
  [0, depths.slice(0, 3).reduce((y, x) => x + y)],
);
console.log(`3-Windowed Increases: ${windowedIncreases}`);
