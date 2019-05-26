open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

module Deck = {
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
        ...List.append(
             cartesianProduct([h1], t2),
             cartesianProduct(t1, l2),
           ),
      ]
    };

  let newDeck = {
    /* A deck contains one of each combinations of these categories, 81 total */
    let colors = [Green, Purple, Red];
    let ranks = [One, Two, Three];
    let suits = [Diamond, Ellipse, Squiggle];
    let fills = [Dashed, None, Solid];

    /* Return a card built from the cartesian product of all four lists */
    cartesianProduct(fills, colors)
    |> cartesianProduct(suits)
    |> cartesianProduct(ranks)
    |> List.map(((r, (s, (f, c)))) => newCard((r, s, f, c)));
  };

  let string_of_card = card => {
    /* placeholder */
    let c =
      switch (card.color) {
      | Green => "g"
      | Purple => "p"
      | Red => "r"
      };
    let r =
      switch (card.rank) {
      | One => "1"
      | Two => "2"
      | Three => "3"
      };
    let s =
      switch (card.suit) {
      | Diamond => "d"
      | Ellipse => "e"
      | Squiggle => "s"
      };
    let f =
      switch (card.fill) {
      | Dashed => "d"
      | None => "n"
      | Solid => "s"
      };

    r ++ "-" ++ c ++ "-" ++ s ++ "-" ++ f;
  };

  let type_of_card = card => /* Each card's type is unique*/ (
    card.rank,
    card.suit,
    card.fill,
    card.color,
  );
};

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
    ~card: Deck.card,
    ~onToggleCard,
    (),
  ) => {
    let toggle = onToggleCard;

    let wrapperStyle =
      Style.[
        border(~width=2, ~color=Colors.white),
        margin(16),
        width(110),
        height(155),
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
      <View style=wrapperStyle>
        <Text style=textValueStyle text={Deck.string_of_card(card)} />
        <Image
          src={Deck.string_of_card(card) ++ ".png"}
          opacity={card.selected ? 0.6 : 1.0}
          width=106
          height=150
        />
      </View>
    </Clickable>;
  };
};

type state = {cards: list(Deck.card)};
type action =
  | ToggleCard((Deck.rank, Deck.suit, Deck.fill, Deck.color))
  | Noop;

let reducer = (action, state) =>
  switch (action) {
  | ToggleCard((r, s, f, c)) => {
      cards:
        List.map(
          card =>
            Deck.type_of_card(card) == (r, s, f, c)
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
                    dispatch(ToggleCard(Deck.type_of_card(card)))
                  }
                />,
              state.cards,
            )}
          </View>
        </View>,
      );
    });
};

let initState = {
  let deck = Deck.newDeck;
  {cards: deck};
};

let init = app => {
  let win = App.createWindow(app, "SET");

  let state = initState;
  let element = <SetGameComponent state />;

  let _ = UI.start(win, element);
  ();
};

App.start(init);