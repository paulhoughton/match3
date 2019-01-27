open SharedTypes;

let component = ReasonReact.statelessComponent("Cell");

let colours = [|"green", "blue", "yellow", "orange"|];

let make = (~cell: cellType, ~selected, ~selectCell, _children) => {
  ...component,
  render: self => {
    <div
      onClick={self.handle((_ev, _s) => selectCell(cell))}
      className={"cell" ++ (selected ? " selected" : "")}
      style={ReactDOMRe.Style.make(~background=colours[cell.cell - 1], ())}
    />;
  },
};
