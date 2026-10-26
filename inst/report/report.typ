// KidVision participant report.
//
// kv_report() fills this template: it writes the data to data.json and the
// charts to PNG files in the same folder, then compiles it with Typst. The
// wording can be edited here; the values come from the `d` variable.

#let d = json("data.json")

#let purple = rgb("#4C1F8D")
#let violet = rgb("#8A5CD0")
#let lavender = rgb("#EFE8FA")
#let ink = rgb("#2B2340")
#let grey = rgb("#6E6880")

#let child = d.child_label
#let childs = d.child_possessive

#set document(title: "KidVision physical activity and sleep report")
#set text(font: "Lato", size: 11pt, fill: ink, lang: "en", region: "AU")
#set par(leading: 0.7em, spacing: 1.2em)
#set page(
  paper: "a4",
  margin: (x: 2.2cm, top: 2.2cm, bottom: 2.4cm),
  footer: context [
    #set text(size: 9pt, fill: grey)
    KidVision #h(1fr) #counter(page).display()
  ],
)

#show heading.where(level: 1): it => block(below: 0.9em)[
  #set text(size: 22pt, fill: purple, weight: "bold")
  #it.body
]
#show heading.where(level: 2): it => block(above: 1.6em, below: 0.8em)[
  #set text(size: 14pt, fill: purple, weight: "bold")
  #it.body
]

// A section heading with one of the round icons.
#let section(icon, title) = grid(
  columns: (34pt, 1fr),
  column-gutter: 10pt,
  align: horizon,
  image(icon, width: 34pt),
  heading(level: 1, title),
)

// A headline number with a short explanation.
#let tile(value, label) = block(
  fill: lavender,
  radius: 10pt,
  inset: 14pt,
  width: 100%,
  height: 82pt,
)[
  #text(size: 20pt, weight: "bold", fill: purple)[
    #if value == none [Not enough data] else [#value]
  ] \
  #text(size: 10pt)[#label]
]

#let notice(body) = block(
  fill: lavender,
  radius: 8pt,
  inset: 12pt,
  width: 100%,
  body,
)

#let not_enough_wear(what) = notice[
  The monitor wasn't worn for long enough to show #what. It needs to be worn
  for at least 16 hours in a day to give a reliable picture.
]

// Cover ----------------------------------------------------------------------

#page(
  margin: 0pt,
  footer: none,
  background: image("front_cover.jpg", width: 100%, height: 100%),
)[
  #place(top + left, dx: 2.2cm, dy: 9.5cm)[
    #set text(fill: white)
    #text(size: 44pt, weight: "bold")[KidVision]
    #v(0.3em)
    #text(size: 20pt)[Physical activity and sleep report]
    #v(1.4em)
    #text(size: 18pt, weight: "bold")[#child]
    #v(0.2em)
    #text(size: 12pt)[#d.timepoint_label · #d.first_day to #d.last_day]
  ]
  #place(bottom + right, dx: -1.8cm, dy: -1.6cm)[
    #image("ACU-LOGO_WHITE.png", width: 4.5cm)
  ]
]

// Thank you ------------------------------------------------------------------

= Thank you!

#if d.parent == none [Dear parent or carer,] else [Dear #d.parent,]

Thank you so much for taking part in KidVision. We truly appreciate the time
and effort that you and #child have given to help us understand more about
children's daily lives.

The information you gave us by wearing #if d.camera [the camera and ] the
activity monitor is incredibly valuable. It will help us learn more about children's physical
activity, sleep, screen time and wellbeing, which could benefit many other
families in the future.

This report shows some of what the activity monitor recorded about #childs
physical activity and sleep. It is not a medical assessment, but we hope it
gives you a general picture of #childs days and nights.

Thank you again for being part of this research.

Warm regards,

#text(size: 14pt, weight: "bold", fill: purple)[The KidVision team]

#v(1fr)

#notice[
  #text(weight: "bold", fill: purple)[About this report] \
  #child wore a GENEActiv activity monitor, which measures movement many
  times a second. We used GGIR, a widely used research tool, to work out the
  time spent asleep, sitting or still, and in light or moderate-to-vigorous
  activity. These results are estimates from a wrist-worn monitor and aren't
  a medical assessment. If you have any concerns about #childs sleep or
  activity, please talk to your GP.
]

#pagebreak()

// At a glance -----------------------------------------------------------------

= At a glance

#child wore the activity monitor on their wrist from #d.first_day to
#d.last_day. On the days and nights it was worn for long enough:

#grid(
  columns: (1fr, 1fr),
  gutter: 12pt,
  tile(
    d.activity.average,
    [moderate-to-vigorous activity on an average day],
  ),
  tile(
    if d.activity.available [#d.activity.days_meeting of #d.activity.days]
    else { none },
    [days with at least 60 minutes of moderate-to-vigorous activity],
  ),
  tile(d.sleep.average, [from falling asleep to waking up, on an average night]),
  tile(
    if d.sleep.available [#d.sleep.bedtime to #d.sleep.wake] else { none },
    [usual time of falling asleep and waking up],
  ),
)

== An average day

#if d.activity.available [
  #image("day.png", width: 100%)

  On an average day, #child spent about #d.day.sleep asleep and
  #d.day.awake_night awake or restless during the night. While awake, they
  spent about #d.day.sitting sitting or still, #d.day.light in light activity
  and #d.day.mvpa in moderate-to-vigorous activity.

  - *Sitting or still* includes things like screen time, reading, schoolwork
    and travelling in the car.
  - *Light activity* includes things like walking around the house, playing
    quietly and helping with chores.
  - *Moderate-to-vigorous activity* makes you breathe harder, like running,
    sport and active play.
] else [
  #not_enough_wear[an average day]
]

#pagebreak()

// Physical activity ---------------------------------------------------------

#section("logoPA.png", [Physical activity])

#if d.activity.available [
  #image("activity.png", width: 100%)

  The chart shows how many minutes of moderate-to-vigorous activity #child did
  each day, with a dashed line at 60 minutes. Days when the monitor was worn
  for less than 16 hours aren't shown.

  The Australian 24-Hour Movement Guidelines recommend that children and young
  people do *at least 60 minutes* of moderate-to-vigorous physical activity
  every day. #child averaged #d.activity.average a day, and reached 60 minutes
  on #d.activity.days_meeting of the #d.activity.days days.

  Keep in mind that the monitor was worn on the wrist, so some activities, such
  as cycling or swimming, may not be fully counted. If #child does a lot of
  these, their activity may be higher than shown here.
] else [
  #not_enough_wear[physical activity]
]

#pagebreak()

// Sleep ------------------------------------------------------------------------

#section("logo_sleep.png", [Sleep])

#if d.sleep.available [
  == How long

  #image("sleep.png", width: 100%)

  Each bar shows a night, from when #child fell asleep to when they woke up.
  The darker part is the time the monitor recorded as sleep, and the lighter
  part is time spent awake or moving restlessly. Because the monitor is on the
  wrist, normal tossing and turning can look like being awake. The shaded band
  shows the recommended amount of sleep.

  #if d.age == none [
    The Australian 24-Hour Movement Guidelines recommend 9 to 11 hours of
    sleep each night for children aged 5 to 13, and 8 to 10 hours for young
    people aged 14 to 17.
  ] else [
    For children aged #d.age, the Australian 24-Hour Movement Guidelines
    recommend *#d.sleep.guideline* of sleep each night.
  ]
  On an average night, #child slept for about #d.sleep.average from falling
  asleep to waking up.

  == When

  #image("timing.png", width: 100%)

  Going to bed and getting up at about the same times each day, including on
  weekends, helps children get good-quality sleep. #child usually fell asleep
  at about #d.sleep.bedtime and woke up at about #d.sleep.wake.

  #pagebreak()

  #section("logo_sleep_quality.png", [How settled were the nights?])

  #image("efficiency.png", width: 100%)

  This shows how much of each night #child spent asleep, out of the time from
  falling asleep to waking up. On an average night it was #d.sleep.efficiency.

  Because the monitor is worn on the wrist, ordinary movement during sleep can
  be counted as being awake, so these numbers are lower than a sleep clinic
  would measure. They are most useful for comparing nights with each other,
  rather than against a target.
] else [
  #not_enough_wear[sleep]
]
