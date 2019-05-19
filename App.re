open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

type rank =
  | One
  | Two
  | Three;

type suit =
  | Diamond
  | Ellipse
  | Squiggle;

type fill =
  | Dashed
  | None
  | Solid;

type color =
  | Green
  | Purple
  | Red;

type card = {
  selected: bool,
  rank,
  suit,
  fill,
  color,
};

let newCard = ((rank, suit, fill, color)) => {
  selected: false,
  rank,
  suit,
  color,
  fill,
};

let rec cartesianProduct = (l1, l2) =>
  /* This is the naive, not tail-recursive implementation */
  switch (l1, l2) {
  | ([], _)
  | (_, []) => []
  | ([h1, ...t1], [h2, ...t2]) => [
      (h1, h2),
      ...List.append(cartesianProduct([h1], t2), cartesianProduct(t1, l2)),
    ]
  };

let newDeck = {
  let colors = [Green, Purple, Red];
  let ranks = [One, Two, Three];
  let suits = [Diamond, Ellipse, Squiggle];
  let fills = [Dashed, None, Solid];

  /* Return the cartesian product of all four lists */
  cartesianProduct(ranks, suits)
  |> cartesianProduct(fills)
  |> cartesianProduct(colors)
  |> List.map(((c, (f, (r, s)))) => (r, s, f, c))
  |> List.map(ct => newCard(ct));
};

let string_of_card = card => {
  /* placeholder */
  let c =
    switch (card.color) {
    | Green => "green"
    | Purple => "purple"
    | Red => "red"
    };
  let r =
    switch (card.rank) {
    | One => "1"
    | Two => "2"
    | Three => "3"
    };
  let s =
    switch (card.suit) {
    | Diamond => "diamonds"
    | Ellipse => "ellipses"
    | Squiggle => "squiggles"
    };
  let f =
    switch (card.fill) {
    | Dashed => "dashed"
    | None => "empty"
    | Solid => "solid"
    };

  "The " ++ f ++ " " ++ c ++ " " ++ r ++ " of " ++ s ++ ".";
};

let type_of_card = card => /* Each card's type is unique*/ (
  card.rank,
  card.suit,
  card.fill,
  card.color,
);

let animatedText = {
  let component = React.component("AnimatedText");

  (~children as _: list(React.syntheticElement), ~delay, ~textContent, ()) =>
    component(hooks => {
      let (translate, hooks) =
        Hooks.animation(
          Animated.floatValue(50.),
          Animated.options(
            ~toValue=0.,
            ~duration=Seconds(0.5),
            ~delay=Seconds(delay),
            (),
          ),
          hooks,
        );

      let (opacityVal: float, hooks) =
        Hooks.animation(
          Animated.floatValue(0.),
          Animated.options(
            ~toValue=1.0,
            ~duration=Seconds(1.),
            ~delay=Seconds(delay),
            (),
          ),
          hooks,
        );

      let textHeaderStyle =
        Style.[
          color(Colors.white),
          fontFamily("Roboto-Regular.ttf"),
          fontSize(24),
          marginHorizontal(8),
          opacity(opacityVal),
          transform([Transform.TranslateY(translate)]),
        ];

      (hooks, <Text style=textHeaderStyle text=textContent />);
    });
};

let cardComponent = {
  let component = React.component("Card");

  (
    ~children as _: list(React.syntheticElement),
    ~card: card,
    ~onToggleCard,
    (),
  ) => {
    let toggle = onToggleCard;

    let wrapperStyleBase =
      Style.[
        backgroundColor(Color.rgba(1., 1., 1., 0.1)),
        border(~width=2, ~color=Colors.white),
        margin(16),
        width(100),
        height(150),
      ];

    let wrapperStyleToggled =
      Style.[
        backgroundColor(Color.rgba(1., 1., 0.8, 0.5)),
        border(~width=2, ~color=Colors.white),
        margin(16),
        width(100),
        height(150),
      ];
    let textColor =
      switch (card.color) {
      | Green => Colors.green
      | Purple => Colors.purple
      | Red => Colors.red
      };
    let textValueStyle =
      Style.[
        color(textColor),
        fontFamily("Roboto-Regular.ttf"),
        fontSize(15),
        margin(4),
      ];
    <Clickable onClick=toggle>
      <View style={card.selected ? wrapperStyleToggled : wrapperStyleBase}>
        <Text style=textValueStyle text={string_of_card(card)} />
      </View>
    </Clickable>;
  };
};

type state = {cards: list(card)};
type action =
  | ToggleCard((rank, suit, fill, color))
  | Noop;

let reducer = (action, state) =>
  switch (action) {
  | ToggleCard((r, s, f, c)) => {
      cards:
        List.map(
          card =>
            type_of_card(card) == (r, s, f, c)
              ? {...card, selected: !card.selected} : card,
          state.cards,
        ),
    }

  | Noop => state
  };

module SetGameComponent = {
  let component = React.component("SetGameComponent");
  let createElement = (~state, ~children as _, ()) =>
    component(hooks => {
      let (state, dispatch, hooks) =
        Hooks.reducer(~initialState=state, reducer, hooks);

      let containerStyle =
        Style.[
          position(`Absolute),
          justifyContent(`Center),
          alignItems(`Center),
          bottom(0),
          top(0),
          left(0),
          right(0),
        ];

      let innerStyle =
        Style.[
          flexDirection(`Row),
          flexWrap(`Wrap),
          alignItems(`FlexStart),
          width(600),
        ];

      (
        hooks,
        <View style=containerStyle>
          <animatedText delay=0.0 textContent="SET" />
          <View style=innerStyle>
            ...{List.map(
              card =>
                <cardComponent
                  card
                  onToggleCard={() =>
                    dispatch(ToggleCard(type_of_card(card)))
                  }
                />,
              state.cards,
            )}
          </View>
        </View>,
      );
    });
};

let init = app => {
  let win = App.createWindow(app, "SET");

  let state = {cards: newDeck};
  let element = <SetGameComponent state />;

  let _ = UI.start(win, element);
  ();
};

App.start(init);