let component = ReasonReact.statelessComponent("Menu");

let make = _children => {
  ...component,
  render: self => <div> (ReasonReact.stringToElement("Menu")) </div>
};