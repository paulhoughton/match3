open SharedTypes;

Random.init(int_of_float(Js.Date.now()));

let size = 10 - 1;

let createCell = (x, y) => {x, y, cell: 1 + Random.int(4)};

let updateCell = (i, updFn) =>
  Array.mapi((j, cell) => i == j ? updFn(cell) : cell);

let fillRow = (x, cells) => {
  let rec fillRowInner = (empty, y, cells) => {
    let {cell} = cells[y][x];

    if (y <= 0) {
      cells;
    } else if (cell == (-1) && empty < 0) {
      fillRowInner(y, y - 1, cells);
    } else if (cell != (-1) && empty >= 0) {
      cells
      |> Array.mapi((i, row) =>
           if (i == empty) {
             updateCell(x, _ => {x, y: empty, cell}, row);
           } else if (y == i) {
             updateCell(x, cell => {...cell, cell: (-1)}, row);
           } else {
             row;
           }
         )
      |> fillRowInner(empty - 1, y - 1);
    } else {
      fillRowInner(empty, y - 1, cells);
    };
  };
  fillRowInner(-1, size, cells);
};

let rec fillCol = (~col=0, cells) => {
  let newCells = fillRow(col, cells);
  col == size ? newCells : fillCol(~col=col + 1, newCells);
};

let replaceCells = (cells, toReplace) => {
  cells
  |> Array.map(
       Array.map(cell =>
         List.exists(p => p == cell, toReplace) ?
           {...cell, cell: (-1)} : cell
       ),
     )
  |> fillCol
  |> Array.mapi((y, row) =>
       Array.mapi(
         (x, cell) => cell.cell == (-1) ? createCell(x, y) : cell,
         row,
       )
     );
};

let rec findAllMatches = (~i=0, ~matched=[], cells) => {
  let rec findLineMatches =
          (~pos=0, ~start=0, ~finish=0, currentMatch, fetcher) => {
    pos > size ?
      currentMatch :
      {
        let rec getMatch = (x, finish) =>
          x > finish ? [] : [fetcher(i, x), ...getMatch(x + 1, finish)];

        let typesMatch = fetcher(i, start).cell == fetcher(i, pos).cell;

        let finish = typesMatch ? pos : finish;

        let matchFound = (pos == size || !typesMatch) && finish - start > 1;

        findLineMatches(
          ~pos=pos + 1,
          ~start=typesMatch ? start : pos,
          ~finish=typesMatch ? finish : pos,
          List.append(
            currentMatch,
            matchFound ? getMatch(start, finish) : [],
          ),
          fetcher,
        );
      };
  };

  let horizontalMatch = findLineMatches(matched, (x, y) => cells[x][y]);
  let matched = findLineMatches(horizontalMatch, (x, y) => cells[y][x]);

  i == size ? matched : findAllMatches(~i=i + 1, ~matched, cells);
};

let rec findMatches = (~matched=[], cells) => {
  let toRemove = findAllMatches(cells);

  List.length(toRemove) == 0 ?
    (cells, List.length(matched)) :
    findMatches(
      replaceCells(cells, toRemove),
      ~matched=List.append(matched, toRemove),
    );
};

let swapCells = (current, selected) => {
  let match = (first, second) => first.x == second.x && first.y == second.y;
  Array.map(
    Array.map(cell =>
      if (match(cell, current)) {
        {...selected, y: current.y, x: current.x};
      } else if (match(cell, selected)) {
        {...current, y: selected.y, x: selected.x};
      } else {
        cell;
      }
    ),
  );
};

let isNeighbour = (a, b) => a <= 1 && b <= 1 && a != b;

let isCellNeighbour = (cell1, cell2) =>
  isNeighbour(abs(cell1.x - cell2.x), abs(cell1.y - cell2.y));

let createBoard = () => {
  let newBoard =
    Array.init(size + 1, y => Array.init(size + 1, x => createCell(x, y)));
  let (board, _matched) = findMatches(newBoard);
  board;
};
