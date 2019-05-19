open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

type card = {
  selected: bool,
  value: int,
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
    let textValueStyle =
      Style.[
        color(Colors.green),
        fontFamily("Roboto-Regular.ttf"),
        fontSize(15),
        margin(4),
      ];
    <Clickable onClick=toggle>
      <View style={card.selected ? wrapperStyleToggled : wrapperStyleBase}>
        <Text style=textValueStyle text={string_of_int(card.value)} />
      </View>
    </Clickable>;
  };
};

let newCard = value => {selected: false, value};

type state = {cards: list(card)};
type action =
  | ToggleCard(int)
  | Noop;

let reducer = (action, state) =>
  switch (action) {
  | ToggleCard(cardVal) => {
      cards:
        List.map(
          card =>
            card.value == cardVal
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
                  onToggleCard={() => dispatch(ToggleCard(card.value))}
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

  let state = {
    cards:
      List.map(v => newCard(v), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
  };
  let element = <SetGameComponent state />;

  let _ = UI.start(win, element);
  ();
};

App.start(init);
