open SharedTypes;

open Logic;

type action =
  | ResetGame
  | SelectCell(cellType);

type state = {
  score: int,
  board: array(array(cellType)),
  selected: option(cellType),
};

let component = ReasonReact.reducerComponent("Game");

let initState = () => {
  {score: 0, board: createBoard(), selected: None};
};

let make = _children => {
  ...component,
  initialState: initState,
  reducer: (action, state) => {
    switch (action) {
    | ResetGame => ReasonReact.Update(initState())
    | SelectCell(target) =>
      switch (state.selected) {
      | None => ReasonReact.Update({...state, selected: Some(target)})
      | Some(selected) =>
        isCellNeighbour(selected, target) ?
          {
            let (board, matched) =
              findMatches(swapCells(target, selected, state.board));

            ReasonReact.Update({
              score: state.score + matched,
              board,
              selected: None,
            });
          } :
          ReasonReact.Update({...state, selected: Some(target)})
      }
    };
  },
  render: self => {
    <>
      <div className="header" onClick={_ev => self.send(ResetGame)}>
        <span> {ReasonReact.string("Match 3")} </span>
        <span>
          {ReasonReact.string(" Score " ++ string_of_int(self.state.score))}
        </span>
      </div>
      <div className="board">
        {ReasonReact.array(
           {self.state.board
            |> Array.map(row =>
                 ReasonReact.array(
                   row
                   |> Array.map(cell =>
                        <Cell
                          selectCell={cell => self.send(SelectCell(cell))}
                          selected={
                            switch (self.state.selected) {
                            | None => false
                            | Some(selected) => cell == selected
                            }
                          }
                          cell
                          key={
                            string_of_int(cell.x) ++ string_of_int(cell.y)
                          }
                        />
                      ),
                 )
               )},
         )}
      </div>
    </>;
  },
};
