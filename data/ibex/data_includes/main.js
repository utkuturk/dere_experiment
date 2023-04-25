PennController.ResetPrefix();
DebugOff();

// TODO
// Add Timers, like 100ms to make the transition smoother

// Get Some demographics and log them in the trial as well

Sequence(
  "ethics",
  "counter",
  "participants",
  "instructions",
  randomize("practice"),
  "start_experiment",
  rshuffle("experiment", "fillers"),
  SendResults(),
  "end"
);

let ready = 1;
let exp = "experiment";
let exp_type = ".csv";
let filler = "fillers";
let prac = "practice";

// **** CONSTANTS **** //
// Introduce a constant to
const myKey = (type) => [newKey(type, " ").wait().log()];

// display a primer that can be clicked away by pressing space bar
const newPrimer = () => [
  newText("primer", "*")
    .css("font-size", "30pt")
    .css("margin-top", "8px")
    .center()
    .print(),
  myKey("primerkey"),
  getText("primer").remove(),
];

Header(
  // Declare global variables to store the participant's ID and demographic information
  newVar("anadil-var").global(),
  newVar("age-var").global()
)
  // Add the particimant info to all trials' results lines
  .log("native", getVar("anadil-var"))
  .log("age", getVar("age-var"));

///////////////////////////

// **** PRE EXPERIMENT **** //

if (ready) {
  // Ethics agreement: participants must agree before continuing
  newTrial(
    "ethics",
    newHtml("consent_form", "consent.html")
      .cssContainer({ width: "720px" })
      .checkboxWarning(
        "Devam etmeden önce çalışmaya katılmayı kabul etmelisiniz."
      )
      .print(),
    newButton("continue", "Devam etmek için tıklayın")
    .settings.css("font-size", "20px")
      .center()
      .print()
      .wait(
        getHtml("consent_form")
          .test.complete()
          .failure(getHtml("consent_form").warn())
      )
  );

  SetCounter("counter", "inc", 1);

  //////////////////////////////////PARTICIPANT INFO

  // Participant information: questions appear as soon as information is input
  newTrial(
    "participants",
    defaultText.center().print(),
    newText(
      "Anadiliniz nedir? <i>(Birden fazla anadiliniz varsa lütfen hepsini yazınız.)</i>"
    ),
    newTextInput("anadil", " ")
      .center()
      .css("margin", "1em") // Add a 1em margin around this element
      .print(),
    newText("Kaç yaşındasınız?"),
    newTextInput("age", " ")
      .center()
      .css("margin", "1em") // Add a 1em margin around this element
      .print(),
    newButton("Devam")
    .settings.css("font-size", "20px")
      .center()
      .print()
      // Only validate a click on Start when inputID has been filled
      .wait(
        newFunction('dummy', ()=>true).test.is(true)
        //MotherTongue
        .and(
        getTextInput("anadil").testNot.text(" ")
                .failure( newText('erroranadil', "Lütfen anadilinizi giriniz.").color("Crimson"), 
                getTextInput("age").text("")))
      .and( getTextInput("age").test.text(/^\d+$/)
                .failure( newText('errorage', "Lütfen yaşınızı rakamla giriniz.").color("Crimson"), 
                          getTextInput("age").text("")))  ),
    // Store the text from inputID into the Var element
    newVar("anadil-var").global().set(getTextInput("anadil")),
    newVar("age-var").global().set(getTextInput("age"))
  );
  
  ////////////////////////////////// INSTRUCTIONS
  newTrial(
    "instructions",
    newHtml("instructions_text", "instructions.html")
      .cssContainer({ margin: "1em" })
      .print(),
    newButton("go_to_exercise", "Alıştırmaya başla")
      .settings.css("font-size", "20px")
      .center()
      .print()
      .wait()
  );
}

////////////////////////////////// PRACTICE ITEMS

Template(prac.concat(".csv"), (row) =>
  newTrial(
    prac,
    newTimer("break", 100).start().wait(),
    newPrimer(),
    newTimer("break2", 100).start().wait(),
    newText("Dialogue1", row.Dialogue1).settings.css("font-size", "20px"),
    newText("Dialogue2", row.Dialogue2).settings.css("font-size", "20px"),
    newCanvas("ContextCanvas", 800, 300)
      .add(100, 0, getText("Dialogue1"))
      .center()
      .add(100, 100, getText("Dialogue2"))
      .center()
      .print(),
    newVar("RTc")
      .global()
      .set(() => Date.now()),
    myKey("Context"),
    getVar("RTc").set((c) => Date.now() - c),
    getCanvas("ContextCanvas").remove(),
    newTimer("break3", 100).start().wait(),
    newController("Amb", "DashedSentence", { s: row.Ambg })
      .settings.css("font-size", "20px")
      .cssContainer({ "margin-left": "25px" })
      .css("white-space","nowrap")
      .print()
      .log()
      .wait()
      .remove(),
    newTimer("break4", 100).start().wait(),
    newVar("RTa")
      .global()
      .set(() => Date.now()),
    newCanvas("QuestionCanvas", 600, 100)
      .add(0, 0, newText(row.Question).settings.css("font-size", "20px"))
      .center()
      .add(
        0,
        60,
        newText("1stOne", "F" + " (" + row.Answer1 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .add(
        100,
        60,
        newText("2ndOne", "J" + " (" + row.Answer2 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .center()
      .print(),
    newSelector("answer")
      .add(getText("1stOne"), getText("2ndOne"))
      .keys("F", "J")
      .log()
      .print()
      .once()
      .wait()
      .test.selected("1stOne")
      .success.apply(
        null,
        [getText("1stOne").css("border-bottom", "5px solid Blue")].concat([
          newHtml(row.A1Cont).center().print(),
        ])
      )
      .failure.apply(
        null,
        [getText("2ndOne").css("border-bottom", "5px solid Blue")].concat([
          newHtml(row.A2Cont).center().print(),
        ])
      ),

    // Wait for feedback and to display which option was selected
    newTimer("wait", 1000).start().wait(),
    getVar("RTa").set((v) => Date.now() - v)
  )
    .log("AnswerRT", getVar("RTa"))
    .log("ContextRT", getVar("RTc"))
    .log("Condition", row.Condition)
    .log("ItemNo", row.ItemNo)
    .log("ItemType", row.Type)
);

////////////////////////////////// START EXPERIMENT
newTrial(
  "start_experiment",
  newText(
    "<h2>Alıştırma turunu tamamladınız. Deneye başlayabilirsiniz.</h2><p> <b>Artık geridönüt almayacaksınız.</b></p><p>Deneye başlamadan önce parmaklarınızı 'boşluk', 'F' ve 'J' tuşlarına yerleştirmenizi, telefonunuzu sessize almanızı ve deneyi tam ekran yapmanızı öneriyoruz. Deneye başlamak için aşağıdaki butona basınız.</p>"
  ).print(),
  newButton("go_to_experiment", "Deneyi başlat").settings.css("font-size", "20px").center().print().wait()
);

////////////////////////////////// EXPERIMENTAL ITEMS

Template(exp.concat(exp_type), (row) =>
  newTrial(
    exp,
    newTimer("break", 100).start().wait(),
    newPrimer(),
    newTimer("break2", 100).start().wait(),
    newText("Dialogue1", row.Dialogue1).settings.css("font-size", "20px"),
    newText("Dialogue2", row.Dialogue2).settings.css("font-size", "20px"),
    newCanvas("ContextCanvas", 800, 300)
      .add(100, 0, getText("Dialogue1"))
      .center()
      .add(100, 100, getText("Dialogue2"))
      .center()
      .print(),
    newVar("RTc")
      .global()
      .set(() => Date.now()),
    myKey("Context"),
    getVar("RTc").set((c) => Date.now() - c),
    getCanvas("ContextCanvas").remove(),
    newTimer("break3", 100).start().wait(),
    newController("Amb", "DashedSentence", { s: row.Ambg })
      .settings.css("font-size", "20px")
      .cssContainer({ "margin-left": "25px" })
      .css("white-space","nowrap")
      .print()
      .log()
      .wait()
      .remove(),
    newTimer("break4", 100).start().wait(),
    newVar("RTa")
      .global()
      .set(() => Date.now()),
    newCanvas("QuestionCanvas", 600, 100)
      .add(0, 0, newText(row.Question).settings.css("font-size", "20px"))
      .center()
      .add(
        0,
        60,
        newText("1stOne", "F" + " (" + row.Answer1 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .add(
        100,
        60,
        newText("2ndOne", "J" + " (" + row.Answer2 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .center()
      .print(),
    newSelector("answer")
      .add(getText("1stOne"), getText("2ndOne"))
      .keys("F", "J")
      .log()
      .print()
      .once()
      .wait(),
    getVar("RTa").set((v) => Date.now() - v)
  )
    .log("AnswerRT", getVar("RTa"))
    .log("ContextRT", getVar("RTc"))
    .log("Condition", row.Condition)
    .log("ItemNo", row.ItemNo)
    .log("ItemType", row.Type)
);

////////////////////////////////// EXPERIMENTAL ITEMS

Template(filler.concat(".csv"), (row) =>
  newTrial(
    filler,
    newTimer("break", 100).start().wait(),
    newPrimer(),
    newTimer("break2", 100).start().wait(),
    newText("Dialogue1", row.Dialogue1).settings.css("font-size", "20px"),
    newText("Dialogue2", row.Dialogue2).settings.css("font-size", "20px"),
    newCanvas("ContextCanvas", 800, 300)
      .add(100, 0, getText("Dialogue1"))
      .center()
      .add(100, 100, getText("Dialogue2"))
      .center()
      .print(),
    newVar("RTc")
      .global()
      .set(() => Date.now()),
    myKey("Context"),
    getVar("RTc").set((c) => Date.now() - c),
    getCanvas("ContextCanvas").remove(),
    newTimer("break3", 100).start().wait(),
    newController("Amb", "DashedSentence", { s: row.Ambg })
      .settings.css("font-size", "20px")
      .cssContainer({ "margin-left": "25px" })
      .css("white-space","nowrap")
      .print()
      .log()
      .wait()
      .remove(),
    newTimer("break4", 100).start().wait(),
    newVar("RTa")
      .global()
      .set(() => Date.now()),
    newCanvas("QuestionCanvas", 600, 100)
      .add(0, 0, newText(row.Question).settings.css("font-size", "20px"))
      .center()
      .add(
        0,
        60,
        newText("1stOne", "F" + " (" + row.Answer1 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .add(
        100,
        60,
        newText("2ndOne", "J" + " (" + row.Answer2 + ")").settings.css(
          "font-weight",
          "bold"
        )
      )
      .center()
      .print(),
    newSelector("answer")
      .add(getText("1stOne"), getText("2ndOne"))
      .keys("F", "J")
      .log()
      .print()
      .once()
      .wait(),
    getVar("RTa").set((v) => Date.now() - v)
  )
    .log("AnswerRT", getVar("RTa"))
    .log("ContextRT", getVar("RTc"))
    .log("Condition", row.Condition)
    .log("ItemNo", row.ItemNo)
    .log("ItemType", row.Type)
);

////////////////////////////////// END SCREEN

newTrial(
  "end",
  newText(
    "<h2>Deneyi tamamladınız. </h2> <br> Cevaplarınız başarıyla kaydedildi."
  )
    .cssContainer({ "margin-top": "1em", "margin-bottom": "1em" })
    .print(),
  newHtml("explain", "end.html").print(),
  // Trick: stay on this trial forever (until tab is closed)
  newButton().wait()
)

.setOption("countsForProgressBar", false);

///////////////////////////

///////////////////////////
