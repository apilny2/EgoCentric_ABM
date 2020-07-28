turtles-own[
  incubation-days
  sick-days
  quarantine-days
  infected
  susceptible
  symptom
  contact
  contact-#
  exposure-time
  contact-history
  infection-history
  infect-#
  contact-level
  #-contact
]
Globals [
  sick
  new-sick
  Cumulative-sick
  #-infected
  R0
  yesterday-sick
  growth-rate
  Cumul-growth-rate
  growth-period
  avg-growth-rate


  cont
  new-contacts
  Cumulative-contacts

  carr
  new-carriers
  Cumulative-carriers
  death
  new-death
  cumulative-death

  quara
  new-quarantined
  Cumulative-quarantined

  current-patient-list
  current-patient-num
  x
  y
  patient-ID
  contact-ID

  first-hand-contact-list
  second-hand-contact-list
  third-hand-contact-list
  fourth-hand-contact-list
  fifth-hand-contact-list
  sixth-hand-contact-list
  seventh-hand-contact-list
  eighth-hand-contact-list
  ninth-hand-contact-list
  tenth-hand-contact-list

  cumul-1st-contacts
  cumul-2nd-contacts
  cumul-3rd-contacts
  cumul-4th-contacts
  cumul-5th-contacts
  cumul-6th-contacts
  cumul-7th-contacts
  cumul-8th-contacts
  cumul-9th-contacts
  cumul-10th-contacts

  first-hand-carrier-list
  second-hand-carrier-list
  third-hand-carrier-list
  fourth-hand-carrier-list
  fifth-hand-carrier-list
  sixth-hand-carrier-list
  seventh-hand-carrier-list
  eighth-hand-carrier-list
  ninth-hand-carrier-list
  tenth-hand-carrier-list
  orphan-contact-list

  first-hand-quarantine-list
  second-hand-quarantine-list
  third-hand-quarantine-list
  fourth-hand-quarantine-list
  fifth-hand-quarantine-list

  cumul-level-quarantined-contacts
  current-contacts-before-quarantine

]
breed [persons person]



to-report susceptible-people     ;call all suscepitble people
  report turtles with [infected = false and susceptible = true and symptom = false and contact = false]
end

to-report contacts
  report turtles with [symptom = false and contact = true]   ;contacts are exposed to infected people but show no symptoms.
end

to-report carriers   ;call all people who are infected but have not shown symptoms
  report turtles with [infected = true and susceptible = false and symptom = false and contact = true]
end

to-report infected-with-symptom   ;call all people who are infected and have shown symptoms
  report turtles with [infected = true and susceptible = false and symptom = true and contact = false]
end


to-report recovered-people   ;call all people who have recovered
  report turtles with [infected = false and susceptible = false and symptom = false and contact = false]
end

to-report average-possible-contact
  report mean [contact-#] of infected-with-symptom
end

to-report average-infection
  report mean [infect-#] of infected-with-symptom
end





;;;;;;;;;;;;;;;;;;;;main setup procedures;;;;;;;;;;;;;;


to setup

  clear-all

  create-persons Population-size        ;create a susceptible population
  [set color 65
    set size 1.6
    set shape "person-1"
    set incubation-days 0
    set sick-days 0
    set quarantine-days 0
    set infected false
    set susceptible true
    set symptom false
    set contact false
    set exposure-time 0
    set contact-history []
    set infection-history []
    setxy random-xcor random-ycor
  ]

  ask one-of persons [          ;introduce the patient 0
    set infected true
    set susceptible false
    set symptom false
    set contact true
    set exposure-time 1
    set color yellow


  ]

  set cumulative-carriers 1  ;count the patient 0
  set contact-ID -1          ;avoid turtle 0
  set yesterday-sick 0
  reset-lists

  reset-ticks
end


;;;;;;;;;;;;;;;;;;;;main go procedures;;;;;;;;;;;;;;

to go-basic-SEIR    ;basic SEIR model

  ;if ticks >= Days [stop]

  if count turtles with [infected = true ] = 0 and ticks > 0 [stop]



    reset-lists
    reset-new-count

    move                 ;basic SEIR model
    incubation
    transmission
    recovery-death

    trace-all-contacts    ;track contact levels

  print-output

  tick

end

to go-basic-SEIR-quarantine    ;basic SEIR model + quarantine

 ; if ticks >= Days [stop]

   if count turtles with [infected = true ] = 0 and ticks > 0 [stop]


    reset-lists
    reset-new-count

    move                 ;Basic SIR model
    incubation
    transmission
    recovery-death

    trace-contacts-quarantine
    calculate-cumul-level-quarantined-contacts
    unquarantine

  print-output

  tick


end


;;;;;;;;;;;;Sub-function procedures;;;;;;;;;;;;;;;;

to move       ;people randomly move around in the simulation
  ask persons with [shape = "person-1"] [forward mobility rt random 360]    ;Keep isolated people from moving
  count-contact
end


to incubation-1     ;Process of developing symptoms

  ask carriers
  [set incubation-days incubation-days + 1                                  ; their infection time increases one day
    if incubation-days >= incubation-period                                 ;if incubation period is reached,
        [set-patient-features set color 25 set shape "isolated-1"           ;Othewise, they become new patients and are isolated
         set sick sick + 1 set Cumulative-sick Cumulative-sick + 1  ]]       ;update statistics of patients

  set new-sick sick ;daily new sick

  if yesterday-sick != 0 [
    set growth-rate (((Cumulative-sick / yesterday-sick) - 1) * 100)
    set growth-period growth-period + 1
    set Cumul-growth-rate (Cumul-growth-rate + growth-rate)
    set avg-growth-rate Cumul-growth-rate / growth-period ]

  set yesterday-sick Cumulative-sick

end

to incubation     ;Process of developing symptoms

  ask carriers
  [set incubation-days incubation-days + 1                                  ; their infection time increases one day
    if incubation-days >= incubation-period                                 ;if incubation period is reached,
      [ifelse random 100 < %-Asymptomatic-Carriers                          ;At the asymptomatic rate
        [if incubation-days > 14 [ set-recovered-features ]]                ;They remain asymptomatic . They recover if they remain asymptomatic for 14 days.
        [set-patient-features set color 25 set shape "isolated-1"           ;Othewise, they become new patients and are isolated
         set sick sick + 1 set Cumulative-sick Cumulative-sick + 1
         set #-infected (#-infected + length infection-history) ]]]       ;update statistics of patients

  set new-sick sick ;daily new sick
  if Cumulative-sick > 0 [set R0 (#-infected / Cumulative-sick)]

  if yesterday-sick != 0 [
    set growth-rate (((Cumulative-sick / yesterday-sick) - 1) * 100)
    set growth-period growth-period + 1
    set Cumul-growth-rate (Cumul-growth-rate + growth-rate)
    set avg-growth-rate Cumul-growth-rate / growth-period ]

  set yesterday-sick Cumulative-sick

end


to transmission       ;Process of transmission
  ask persons with [infected = true and symptom = false and shape = "person-1"];                                 ; Ask the unquarantined carriers. Use the shape of "person-1" to exclude quarantined people
  [let contact-persons other persons in-radius Infection-radius with [susceptible = true and shape = "person-1"]   ; Find out the susceptible people in radius of Infection-radius. Use the shape of "person-1" to exclude the quarantined people
    if contact-persons != nobody
    [set contact-history sentence contact-history [who] of contact-persons                                       ;update contact history
     set contact-history sort remove-duplicates contact-history                                                      ;remove duplications from updated list
     set contact-# length contact-history
      ask contact-persons [
      if contact = false [set cont cont + 1 set Cumulative-contacts Cumulative-contacts + 1 set contact true]   ;update new contacts
      set exposure-time exposure-time + 1                                                                       ;update exposure time
        set color 125]                                                                                             ;update color

    if random 100 < Transmission-rate [
     let unlucky one-of contact-persons                                                                        ;pick one person from susceptible contacts
       if unlucky != nobody [
          ask unlucky [set-carrier-features]                                                                                 ;turn the person to a carrier
          set carr carr + 1 set Cumulative-carriers Cumulative-carriers + 1
          set infection-history lput [who] of unlucky infection-history                                                             ;update infection history
          set infect-# length infection-history
  ]]]]


  set new-contacts cont   ;Daily new contacts. This does not count those have been exposed.
  set new-carriers carr   ;Daily new carriers

end


to recovery-death   ;Process of recovery/death
   ask persons with [infected = true and symptom = true]                            ;ask the sick people
     [ifelse sick-days < 10                                                         ; It takes 10-13 days to recover (CDC)
      [set sick-days sick-days + 1  if sick-days > 1 [set color 22]]                ;differentiate new and old patients
      [ifelse random 100 < 2                                                        ; set mortality as 2%
        [set death death + 1 set cumulative-death cumulative-death + 1 die  ]       ;die
        [set-recovered-features set shape "person-1" setxy random-xcor random-ycor] ;recover
      ]
     ]
  set new-death death  ;daily new death

end


to trace-all-contacts
 (ifelse
  Patient-Groups = "All Patients"
    [set current-patient-list sort [who] of persons with [symptom = true]
      set current-patient-num length current-patient-list]

  Patient-Groups ="New Patients only"
    [set current-patient-list sort [who] of persons with [symptom = true and sick-days = 1 + Quarantine-Time-lags]  ;first identify all current new patients and log the IDs in order
     set current-patient-num length current-patient-list])                                     ;detemine the number of new patients


  trace-1st-hand-contacts    ;trace 1st-hand contacts
  trace-2nd-hand-contacts    ;trace 2nd-hand contacts
  trace-3rd-hand-contacts    ;trace 3rd-hand contacts
  trace-4th-hand-contacts    ;trace 4th-hand contacts
  trace-5th-hand-contacts    ;trace 5th-hand contacts
  trace-6th-hand-contacts    ;trace 6th-hand contacts
  ;trace-7th-hand-contacts

  set orphan-contact-list [who] of contacts with [contact-level = 0]

end


to trace-contacts-quarantine
 (ifelse
  Patient-Groups = "All Patients"
    [set current-patient-list sort [who] of persons with [symptom = true]
      set current-patient-num length current-patient-list]

  Patient-Groups ="New Patients only"
    [set current-patient-list sort [who] of persons with [symptom = true and sick-days = 1 + Quarantine-Time-lags]  ;first identify all current new patients and log the IDs in order
     set current-patient-num length current-patient-list])                                     ;detemine the number of new patients


  trace-1st-hand-contacts   ;trace 1st-hand contacts

 (ifelse
   Levels-of-Contact = "1st-hand"  ;Quarantine up to level-1 contacts
    [quarantine-1st-hand-contacts
     trace-2nd-hand-contacts    ;trace 2nd-hand contacts
     trace-3rd-hand-contacts    ;trace 3rd-hand contacts
     trace-4th-hand-contacts    ;trace 4th-hand contacts
     trace-5th-hand-contacts    ;trace 5th-hand contacts
      trace-6th-hand-contacts    ;trace 6th-hand contacts
      trace-7th-hand-contacts]


   Levels-of-Contact = "2nd-hand"   ;Quarantine up to level-2 contacts
    [quarantine-1st-hand-contacts
     trace-2nd-hand-contacts
     quarantine-2nd-hand-contacts
     trace-3rd-hand-contacts    ;trace 3rd-hand contacts
     trace-4th-hand-contacts    ;trace 4th-hand contacts
     trace-5th-hand-contacts    ;trace 5th-hand contacts
     trace-6th-hand-contacts   ;trace 6th-hand contacts
      trace-7th-hand-contacts]

   Levels-of-Contact = "3rd-hand"   ;Quarantine up to level-3 contacts
    [quarantine-1st-hand-contacts
     trace-2nd-hand-contacts
     quarantine-2nd-hand-contacts
     trace-3rd-hand-contacts
     quarantine-3rd-hand-contacts
     trace-4th-hand-contacts
     trace-5th-hand-contacts
     trace-6th-hand-contacts]

  Levels-of-Contact = "4th-hand"   ;Quarantine up to level-4 contacts
    [quarantine-1st-hand-contacts
     trace-2nd-hand-contacts
     quarantine-2nd-hand-contacts
     trace-3rd-hand-contacts
     quarantine-3rd-hand-contacts
     trace-4th-hand-contacts
     quarantine-4th-hand-contacts
     trace-5th-hand-contacts
     trace-6th-hand-contacts
      trace-7th-hand-contacts]

   Levels-of-Contact = "5th-hand"   ;Quarantine up to level-5 contacts
    [quarantine-1st-hand-contacts
     trace-2nd-hand-contacts
     quarantine-2nd-hand-contacts
     trace-3rd-hand-contacts
     quarantine-3rd-hand-contacts
     trace-4th-hand-contacts
     quarantine-4th-hand-contacts
     trace-5th-hand-contacts
     quarantine-5th-hand-contacts
     trace-6th-hand-contacts
      trace-7th-hand-contacts])

  set orphan-contact-list [who] of contacts with [contact-level = 0 and shape = "person-1"]
  set current-contacts-before-quarantine current-contacts-before-quarantine + length orphan-contact-list

end

to unquarantine
  ask contacts with [shape = "isolated-1"]                        ; ask qurantined contacts
      [ifelse quarantine-days < 14                                ;if not 14 days,
        [set quarantine-days quarantine-days + 1]                 ;update quarantine days
        [set shape "person-1" set quarantine-days 0               ;at 14th quanratine day, unquanrantine the person (change the shape) and refresh quanrantine days
          if infected = false [set color 65 set contact false]   ;if the contact is susceptible, set back as susceptible after quarantine
         ]
       ]

end



;;;;;;;;;;;;Supporting procedures;;;;;;;;;;;;;;;


to reset-new-count
  set sick 0
  set cont 0
  set carr 0
  set death 0
  set quara 0
  set current-contacts-before-quarantine 0
end

to set-recovered-features
  set infected false
  set susceptible false
  set symptom false
  set contact false
  set color blue
  set incubation-days 0
end

to set-patient-features
  set infected true
  set susceptible false
  set symptom true
  set contact false
  set incubation-days 0
end

to set-carrier-features
  set infected true
  set susceptible false
  set symptom false
  set color yellow
end

to set-quaratine
  set shape "isolated-1" set quara quara + 1 set Cumulative-quarantined Cumulative-quarantined + 1
end

to print-output
  output-print word "Day" (ticks + 1)
  output-print word "current-patients:" sort current-patient-list
  output-print word "current-1st-hand-contacts:" first-hand-contact-list
  output-print word "1st-hand carriers:" sort first-hand-carrier-list
  output-print word "2nd-hand carriers:" sort second-hand-carrier-list
  output-print word "3rd-hand carriers:" sort third-hand-carrier-list
  output-print word "4th-hand carriers:" sort fourth-hand-carrier-list
  output-print word "5th-hand carriers:" sort fifth-hand-carrier-list
  output-print word "6th-hand carriers:" sort sixth-hand-carrier-list
  output-print word "Untraced Contacts:" sort orphan-contact-list
  output-print word "Contact History of patients:" [contact-history] of infected-with-symptom
  output-print ""


end

to reset-lists
  set current-patient-list []
  set first-hand-carrier-list []
  set second-hand-carrier-list []
  set third-hand-carrier-list []
  set fourth-hand-carrier-list []
  set fifth-hand-carrier-list []
  set sixth-hand-carrier-list []
  set seventh-hand-carrier-list []
  set orphan-contact-list []
  set first-hand-contact-list []
  set second-hand-contact-list []
  set third-hand-contact-list []
  set fourth-hand-contact-list []
  set fifth-hand-contact-list []
  set sixth-hand-contact-list []
  set seventh-hand-contact-list []
  set first-hand-quarantine-list []
  set second-hand-quarantine-list []
  set third-hand-quarantine-list []
  set fourth-hand-quarantine-list []
  set fifth-hand-quarantine-list []
  ask persons [set contact-level 0]

end

to trace-1st-hand-contacts     ;trace 1st-hand contacts
  set x 0
  if not empty? current-patient-list [                                                    ;if current-patient-list is not empty
    Repeat length current-patient-list [                                                  ;Use this number of new patients to loop procedure
       set patient-ID item x current-patient-list                                         ;find the paitent ID
        ask persons with [who = patient-ID]                                                                     ;ask unquarantined contacts
          [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and shape = "person-1"]
               [set contact-level 1   set first-hand-contact-list lput who first-hand-contact-list         ;set your contact level
                if infected = true [                                                            ;if you are infected
                  set first-hand-carrier-list lput who first-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set first-hand-carrier-list sort remove-duplicates first-hand-carrier-list
  set first-hand-contact-list sort remove-duplicates first-hand-contact-list
  set cumul-1st-contacts cumul-1st-contacts + length first-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length first-hand-contact-list

end

to trace-2nd-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? first-hand-carrier-list [
     Repeat length first-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x first-hand-carrier-list                                         ;find the carrier ID
      ask persons with [who = patient-ID ]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 2  set second-hand-contact-list lput who second-hand-contact-list                                                           ;set your contact level
              if infected = true [                      ;if you are infected
                  set second-hand-carrier-list lput who second-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
    set second-hand-carrier-list sort remove-duplicates second-hand-carrier-list
    set second-hand-contact-list sort remove-duplicates second-hand-contact-list
    set cumul-2nd-contacts cumul-2nd-contacts + length second-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length second-hand-contact-list

end

to trace-3rd-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? second-hand-carrier-list [
     Repeat length second-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x second-hand-carrier-list                                         ;find the carrier ID
      ask persons with [who = patient-ID]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 3 set third-hand-contact-list lput who third-hand-contact-list                                                         ;set your contact level
              if infected = true [                      ;if you are infected
                  set third-hand-carrier-list lput who third-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set third-hand-carrier-list sort remove-duplicates third-hand-carrier-list
  set third-hand-contact-list sort remove-duplicates third-hand-contact-list
  set cumul-3rd-contacts cumul-3rd-contacts + length third-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length third-hand-contact-list

end

to trace-4th-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? third-hand-carrier-list [
     Repeat length third-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x third-hand-carrier-list                                         ;find the carrier ID
      ask persons with [who = patient-ID]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 4 set fourth-hand-contact-list lput who fourth-hand-contact-list                                                            ;set your contact level
              if infected = true [                      ;if you are infected
                  set fourth-hand-carrier-list lput who fourth-hand-carrier-list
                  ]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set fourth-hand-carrier-list sort remove-duplicates fourth-hand-carrier-list
  set fourth-hand-contact-list sort remove-duplicates fourth-hand-contact-list
  set cumul-4th-contacts cumul-4th-contacts + length fourth-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length fourth-hand-contact-list

end

to trace-5th-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? fourth-hand-carrier-list [
     Repeat length fourth-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x fourth-hand-carrier-list                                         ;find the carrier ID
      ask persons with [who = patient-ID]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 5 set fifth-hand-contact-list lput who fifth-hand-contact-list                                                            ;set your contact level
              if infected = true [                      ;if you are infected
                  set fifth-hand-carrier-list lput who fifth-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set fifth-hand-carrier-list sort remove-duplicates fifth-hand-carrier-list
  set fifth-hand-contact-list sort remove-duplicates fifth-hand-contact-list
  set cumul-5th-contacts cumul-5th-contacts + length fifth-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length fifth-hand-contact-list

end

to trace-6th-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? fifth-hand-carrier-list [
     Repeat length fifth-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x fifth-hand-carrier-list                                        ;find the carrier ID
      ask persons with [who = patient-ID]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 6  set sixth-hand-contact-list lput who sixth-hand-contact-list                                                             ;set your contact level
              if infected = true [                      ;if you are infected
                  set sixth-hand-carrier-list lput who sixth-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set sixth-hand-carrier-list sort remove-duplicates sixth-hand-carrier-list
  set sixth-hand-contact-list sort remove-duplicates sixth-hand-contact-list
  set cumul-6th-contacts cumul-6th-contacts + length sixth-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length sixth-hand-contact-list

end

to trace-7th-hand-contacts
  set contact-ID -1
  set patient-ID -1
  set x 0
  set y 0
  if not empty? sixth-hand-carrier-list [
     Repeat length sixth-hand-carrier-list [                                                 ;Use this number to loop procedure
       set patient-ID item x sixth-hand-carrier-list                                        ;find the carrier ID
      ask persons with [who = patient-ID]                                                                        ;ask unquarantined contacts
        [if not empty? contact-history
           [Repeat length contact-history
             [set contact-ID item y contact-history                                            ;if you were exposed to this patient
              ask contacts with [who = contact-ID and contact-level = 0 and shape = "person-1"]
             [set contact-level 7  set seventh-hand-contact-list lput who seventh-hand-contact-list                                                             ;set your contact level
              if infected = true [                      ;if you are infected
                  set seventh-hand-carrier-list lput who seventh-hand-carrier-list]]                 ;add to carrier list
              set y y + 1]
              set y 0]]
      set x x + 1]]
  set seventh-hand-carrier-list sort remove-duplicates seventh-hand-carrier-list
  set seventh-hand-contact-list sort remove-duplicates seventh-hand-contact-list
  set cumul-7th-contacts cumul-7th-contacts + length seventh-hand-contact-list
  set current-contacts-before-quarantine current-contacts-before-quarantine + length seventh-hand-contact-list

end


to quarantine-1st-hand-contacts
  ask up-to-n-of (%-Contacts-Quarantined * count contacts with [contact-level = 1 and shape = "person-1" ] / 100) contacts with [contact-level = 1 and shape = "person-1" ] ;find XX% of level-1 contacts
     [set first-hand-quarantine-list lput who first-hand-quarantine-list    ;log who to form a quarantine list
      set-quaratine                                                         ;quarantine it
      if infected = true
       [set first-hand-carrier-list lput who first-hand-carrier-list]       ;Log the carriers
      ]
     set new-quarantined quara                                              ; update new quarantined contacts
     set first-hand-carrier-list sort remove-duplicates first-hand-carrier-list  ;clean up the level-1 carrier list
end

to quarantine-2nd-hand-contacts
    ask up-to-n-of (%-Contacts-Quarantined * count contacts with [contact-level = 2 and shape = "person-1" ] / 100) contacts with [contact-level = 2 and shape = "person-1" ] ;find XX% of level-2 contacts
     [set second-hand-quarantine-list lput who second-hand-quarantine-list    ;log who to form a quarantine list
      set-quaratine                                                         ;quarantine it
      if infected = true
       [set second-hand-carrier-list lput who second-hand-carrier-list]       ;Log the carriers
      ]
     set new-quarantined quara                                              ; update new quarantined contacts
     set second-hand-carrier-list sort remove-duplicates second-hand-carrier-list  ;clean up the level-2 carrier list
end

to quarantine-3rd-hand-contacts
    ask up-to-n-of (%-Contacts-Quarantined * count contacts with [contact-level = 3 and shape = "person-1" ] / 100) contacts with [contact-level = 3 and shape = "person-1" ] ;find XX% of level-3 contacts
     [set third-hand-quarantine-list lput who third-hand-quarantine-list    ;log who to form a quarantine list
      set-quaratine                                                         ;quarantine it
      if infected = true
       [set third-hand-carrier-list lput who third-hand-carrier-list]       ;Log the carriers
      ]
     set new-quarantined quara                                              ; update new quarantined contacts
     set third-hand-carrier-list sort remove-duplicates third-hand-carrier-list  ;clean up the level-3 carrier list
end

to quarantine-4th-hand-contacts
    ask up-to-n-of (%-Contacts-Quarantined * count contacts with [contact-level = 4 and shape = "person-1" ] / 100) contacts with [contact-level = 4 and shape = "person-1" ] ;find XX% of level-4 contacts
     [set fourth-hand-quarantine-list lput who fourth-hand-quarantine-list    ;log who to form a quarantine list
      set-quaratine                                                         ;quarantine it
      if infected = true
       [set fourth-hand-carrier-list lput who fourth-hand-carrier-list]       ;Log the carriers
      ]
     set new-quarantined quara                                              ; update new quarantined contacts
     set fourth-hand-carrier-list sort remove-duplicates fourth-hand-carrier-list  ;clean up the level-4 carrier list
end

to quarantine-5th-hand-contacts
    ask up-to-n-of (%-Contacts-Quarantined * count contacts with [contact-level = 5 and shape = "person-1" ] / 100) contacts with [contact-level = 5 and shape = "person-1" ] ;find XX% of level-5 contacts
     [set fifth-hand-quarantine-list lput who fifth-hand-quarantine-list    ;log who to form a quarantine list
      set-quaratine                                                         ;quarantine it
      if infected = true
       [set fifth-hand-carrier-list lput who fifth-hand-carrier-list]       ;Log the carriers
      ]
     set new-quarantined quara                                              ; update new quarantined contacts
     set fifth-hand-carrier-list sort remove-duplicates fifth-hand-carrier-list  ;clean up the level-4 carrier list
end

to calculate-cumul-level-quarantined-contacts
   if cumul-1st-contacts > 0
  [(ifelse
    levels-of-contact = "1st-hand"
        [set cumul-level-quarantined-contacts Cumulative-quarantined * 100 / cumul-1st-contacts]
    levels-of-contact = "2nd-hand"
        [set cumul-level-quarantined-contacts Cumulative-quarantined * 100 / (cumul-1st-contacts + cumul-2nd-contacts) ]
    levels-of-contact = "3rd-hand"
        [set cumul-level-quarantined-contacts Cumulative-quarantined * 100 / (cumul-1st-contacts + cumul-2nd-contacts + cumul-3rd-contacts) ]
    levels-of-contact = "4th-hand"
        [set cumul-level-quarantined-contacts Cumulative-quarantined * 100 / (cumul-1st-contacts + cumul-2nd-contacts + cumul-3rd-contacts + cumul-4th-contacts)]
    levels-of-contact = "5th-hand"
    [set cumul-level-quarantined-contacts Cumulative-quarantined * 100 / (cumul-1st-contacts + cumul-2nd-contacts + cumul-3rd-contacts + cumul-4th-contacts + cumul-5th-contacts)])]
end


to count-contact
  ask turtles [
    let contact-1 other turtles in-radius Infection-radius
    set #-contact count contact-1]
end
@#$#@#$#@
GRAPHICS-WINDOW
224
13
598
388
-1
-1
6.0
1
10
1
1
1
0
1
1
1
-30
30
-30
30
1
1
1
Days
5.0

BUTTON
608
14
701
82
Set up/Reset
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
708
14
874
47
Basic SEIR model
go-basic-SEIR
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
9
10
211
43
Population-size
Population-size
500
10000
1000.0
500
1
NIL
HORIZONTAL

BUTTON
708
49
874
82
Basic SEIR model-Run a Day
go-basic-SEIR\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
9
47
211
80
Mobility
Mobility
0
4
2.0
0.5
1
NIL
HORIZONTAL

TEXTBOX
12
300
140
320
|Green: Susceptible\n
10
54.0
1

TEXTBOX
12
316
132
334
|Orange: New Patients
10
25.0
1

TEXTBOX
12
375
162
393
|Blue: Recovered/Immunized
10
105.0
1

TEXTBOX
12
361
188
380
|Yellow: Infected Contact (Carriers)
10
44.0
1

SLIDER
9
195
210
228
%-Contacts-Quarantined
%-Contacts-Quarantined
0
100
75.0
5
1
NIL
HORIZONTAL

SLIDER
9
85
210
118
Transmission-rate
Transmission-rate
0
100
45.0
1
1
NIL
HORIZONTAL

SLIDER
9
121
210
154
Incubation-period
Incubation-period
2
14
8.0
1
1
NIL
HORIZONTAL

TEXTBOX
12
345
186
364
|Magenta: Contacts not infected
10
125.0
1

MONITOR
693
183
813
220
Culmul # Patients
Cumulative-sick
17
1
9

MONITOR
610
223
689
260
New Contacts
new-contacts
17
1
9

MONITOR
693
223
813
260
Cumul # Contacts
Cumulative-contacts
17
1
9

MONITOR
610
259
689
296
New Carriers
new-carriers
17
1
9

MONITOR
693
259
813
296
Cumul # Carriers
Cumulative-carriers
17
1
9

MONITOR
610
299
689
336
New Deaths
new-death
17
1
9

MONITOR
693
299
813
336
Cumul # Deaths
cumulative-death
17
1
9

MONITOR
813
223
920
260
Cumu % Contacts
Cumulative-contacts * 100 / population-size
2
1
9

MONITOR
813
259
920
296
Cumul % Carriers
Cumulative-carriers * 100 / population-size
2
1
9

MONITOR
813
299
920
336
Cumul % Deaths
cumulative-death * 100 / population-size
2
1
9

PLOT
923
183
1171
340
Daily Updates
Days
Freq
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Patient" 1.0 0 -955883 true "" "plot new-sick"
"Contact" 1.0 0 -8630108 true "" "plot new-contacts"
"Carrier" 1.0 0 -4079321 true "" "plot new-carriers"
"Death" 1.0 0 -7500403 true "" "plot new-death"
"Quarantined" 1.0 0 -13791810 true "" "plot new-quarantined"

PLOT
1173
183
1421
340
Cumulative Updates
Days
Freq
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Patient" 1.0 0 -955883 true "" "plot cumulative-sick * 100 / population-size"
"Contact" 1.0 0 -8630108 true "" "plot cumulative-contacts * 100 / population-size"
"Carrier" 1.0 0 -4079321 true "" "plot cumulative-carriers * 100 / population-size"
"Death" 1.0 0 -7500403 true "" "plot cumulative-death * 100 / population-size"
"Quarantined" 1.0 0 -13791810 true "" "plot Cumulative-quarantined * 100 / Population-size"

MONITOR
813
183
920
220
Cumul % patients
Cumulative-sick * 100 / population-size
2
1
9

OUTPUT
10
603
299
741
9

MONITOR
610
183
689
220
New Patients
new-sick
17
1
9

SLIDER
9
158
210
191
Infection-radius
Infection-radius
1
5
4.0
1
1
NIL
HORIZONTAL

TEXTBOX
12
331
162
349
|Dark red: Old Patients
10
22.0
1

MONITOR
1088
138
1252
175
Avg Susceptible Contacts
average-possible-contact
1
1
9

BUTTON
880
14
1092
47
Basic SEIR & Quarantine
go-basic-SEIR-quarantine\n\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
1053
410
1422
569
Changes in number of contacts at diff Contact Levels
Xth-hand Contacts
# of Contacts
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"1st" 1.0 0 -7500403 true "" "plot count contacts with [contact-level = 1]"
"2nd" 1.0 0 -2674135 true "" "plot count contacts with [contact-level = 2]"
"3rd" 1.0 0 -955883 true "" "plot count contacts with [contact-level = 3]"
"4th" 1.0 0 -6459832 true "" "plot count contacts with [contact-level = 4]"
"5th" 1.0 0 -1184463 true "" "plot count contacts with [contact-level = 5]"
"6th" 1.0 0 -10899396 true "" "plot count contacts with [contact-level = 6]"
"7th" 1.0 0 -13840069 true "" "plot count contacts with [contact-level = 7]"

CHOOSER
770
93
908
138
Levels-of-Contact
Levels-of-Contact
"1st-hand" "2nd-hand" "3rd-hand" "4th-hand" "5th-hand"
2

CHOOSER
610
93
766
138
Patient-Groups
Patient-Groups
"All Patients" "New Patients only"
1

BUTTON
880
49
1094
83
Basic SEIR & Quarantine-Run a Day
go-basic-SEIR-quarantine\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
309
588
443
625
# of 1st-hand Contacts
count contacts with [contact-level = 1]\n\n;Report all 1st-hand contacts in this tick, \n;including both quarantined and unquarantined.
17
1
9

MONITOR
309
628
442
665
# of 2nd-hand Contacts
count contacts with [contact-level = 2]
17
1
9

MONITOR
309
668
442
705
# of 3rd-hand Contacts
count contacts with [contact-level = 3]
17
1
9

MONITOR
309
705
442
742
# of 4th-hand Contacts
count contacts with [contact-level = 4]
17
1
9

MONITOR
1263
138
1422
175
Average Infections
Mean [infect-#] of infected-with-symptom
3
1
9

SLIDER
10
266
211
299
Quarantine-Time-lags
Quarantine-Time-lags
0
7
0.0
1
1
day(s)
HORIZONTAL

MONITOR
10
401
140
438
Daily Growth Rate
growth-rate
2
1
9

MONITOR
147
400
298
437
Average Growth Rate
avg-growth-rate
2
1
9

PLOT
10
438
298
593
Patient Growth Rates
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Daily GR" 1.0 0 -16777216 true "" "plot growth-rate"
"Avg GR" 1.0 0 -2674135 true "" "plot avg-growth-rate"

MONITOR
610
342
707
379
New Quarantined
new-quarantined
17
1
9

MONITOR
708
342
830
379
Cumul # Quarantined
Cumulative-quarantined
17
1
9

MONITOR
832
342
1074
379
Cumul % Quarantined (by entire population)
Cumulative-quarantined * 100 / Population-size
2
1
9

MONITOR
308
528
556
565
Cumul % quarantined contacts by Cumul Contacts
Cumulative-quarantined * 100 / Cumulative-contacts
2
1
9

MONITOR
444
628
612
665
Quarantined 2nd-hand contacts
count contacts with [contact-level = 2 and shape != \"person-1\"]
2
1
9

MONITOR
444
588
613
625
Quarantined 1st-hand contacts
count contacts with [contact-level = 1 and shape != \"person-1\"]
17
1
9

MONITOR
444
665
612
702
Quarantined 3rd-hand contacts
count contacts with [contact-level = 3 and shape != \"person-1\"]
17
1
9

MONITOR
444
705
612
742
Quarantined 4th-hand contacts
count contacts with [contact-level = 4 and shape != \"person-1\"]
17
1
9

MONITOR
614
588
792
625
% quarantined 1st-hand contacts
count contacts with [contact-level = 1 and shape != \"person-1\"] * 100 / count contacts with [contact-level = 1]
2
1
9

MONITOR
614
628
792
665
% quarantined 2nd-hand contacts
count contacts with [contact-level = 2 and shape != \"person-1\"] * 100  / count contacts with [contact-level = 2]
2
1
9

MONITOR
614
668
792
705
% quarantined 3rd-hand contacts
count contacts with [contact-level = 3 and shape != \"person-1\"] * 100 / count contacts with [contact-level = 3]
2
1
9

MONITOR
614
705
792
742
% quarantined 4th-hand contacts
count contacts with [contact-level = 4 and shape != \"person-1\"] * 100  / count contacts with [contact-level = 4]
2
1
9

MONITOR
1173
343
1315
380
Overall Attack Rate
(Cumulative-carriers - 1) / Cumulative-contacts
3
1
9

MONITOR
1317
343
1423
380
2nd Attack 
Mean [infect-#] of infected-with-symptom / average-possible-contact
3
1
9

MONITOR
308
488
556
525
Cumul % quarantined contacts by defined levels
cumul-level-quarantined-contacts\n\n;report the cumulative percent of quarantined contacts\n;given a level of contact and %-contact-quarantined
2
1
9

MONITOR
309
743
443
780
# of 5th-hand Contacts
count contacts with [contact-level = 5]
17
1
9

MONITOR
444
743
612
780
Quarantined 5th-hand contacts
count contacts with [contact-level = 5 and shape != \"person-1\"]
17
1
9

MONITOR
614
743
793
780
% quarantined 5th-hand contacts
count contacts with [contact-level = 5 and shape != \"person-1\"] * 100  / count contacts with [contact-level = 5]
2
1
9

MONITOR
308
408
556
445
Daily contacts before quarantine occurs
current-contacts-before-quarantine
17
1
9

MONITOR
308
448
556
485
Daily % Quarantined Contacts
new-quarantined * 100 / current-contacts-before-quarantine\n\n;Percent of contacts quarantined based on total daily contacts\n;including the traced and untraced contacts\n;exluding contacts who have been quarantined\n;Therefore, it gives a measure on the percent of contacts are actually quarantined\n;given a certain %-contacts-quarantined and a certain level of contact each day.
2
1
9

TEXTBOX
608
164
1084
182
*The contacts in all monitors and plots consist of both infected and not infected contacts.
10
14.0
1

PLOT
573
408
1037
568
Percentages of Quarantined Contacts
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Daily %" 1.0 0 -7500403 true "" "if current-contacts-before-quarantine > 0 \n[plot new-quarantined * 100 / current-contacts-before-quarantine]"
"Cumul % by levels" 1.0 0 -2674135 true "" "plot cumul-level-quarantined-contacts"
"Cumul % by Cumul Contacts" 1.0 0 -8630108 true "" "if Cumulative-contacts > 0\n[ plot Cumulative-quarantined * 100 / Cumulative-contacts ]"
"Cumul % by Population" 1.0 0 -13791810 true "" "plot Cumulative-quarantined * 100 / Population-size"

PLOT
800
589
1043
709
Distribution of Number of contacts
NIL
NIL
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [#-contact] of infected-with-symptom"

MONITOR
1051
746
1173
791
average contacts
mean [#-contact] of turtles
2
1
11

PLOT
1051
590
1263
740
Average number of contact
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [#-contact] of turtles"

PLOT
800
712
1042
832
Distribution of Contact Levels
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [contact-level] of contacts"

SLIDER
9
231
211
264
%-Asymptomatic-Carriers
%-Asymptomatic-Carriers
0
100
90.0
1
1
NIL
HORIZONTAL

BUTTON
1052
796
1172
829
NIL
count-contact
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1163
85
1220
130
NIL
R0
3
1
11

PLOT
1223
10
1423
130
Estimated R0
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot R0"

@#$#@#$#@
Under construction...
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bed
false
0
Rectangle -7500403 false true 45 15 255 285

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -1 true false 45 150 255 285
Rectangle -16777216 true false 128 210 173 285
Polygon -1 true false 45 150 150 60 255 150
Line -16777216 false 255 150 150 60
Line -16777216 false 45 150 150 60
Polygon -7500403 true true 150 30 0 165 15 180 150 60 285 180 300 165
Rectangle -7500403 true true 240 45 255 120

house-2
false
0
Rectangle -1 true false 45 150 255 285
Rectangle -16777216 true false 120 225 180 285
Polygon -7500403 true true 0 150 150 30 300 150
Line -16777216 false 0 150 300 150
Rectangle -7500403 true true 210 45 255 120

isolated
false
2
Circle -7500403 false false 0 0 300
Circle -955883 true true 120 0 60
Polygon -955883 true true 150 75 135 75 120 75 75 165 90 180 120 120 120 180 90 270 120 300 150 195 180 300 210 270 180 180 180 120 210 180 225 165 180 75

isolated-1
false
2
Polygon -955883 false true 150 0 0 120 15 135 45 105 45 300 255 300 255 105 285 135 300 120
Circle -955883 true true 120 15 60
Polygon -955883 true true 165 90 135 90 120 75 75 165 90 180 120 120 120 180 90 270 120 300 150 195 180 300 210 270 180 180 180 120 210 180 225 165 180 75

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

patient
false
0
Rectangle -7500403 false true 0 45 300 255
Circle -7500403 true true 15 120 60
Polygon -7500403 true true 90 195 180 180 285 210 300 195 300 180 210 150 300 120 300 105 270 90 180 120 90 105
Rectangle -7500403 true true 75 135 94 165
Polygon -7500403 true true 105 105 150 60 180 75 105 135
Polygon -7500403 true true 90 180 150 240 180 225 105 165

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 120 15 60
Polygon -7500403 true true 105 90 120 180 90 285 105 300 120 300 150 225 180 300 195 300 210 285 180 180 195 90
Rectangle -7500403 true true 135 75 165 94
Polygon -7500403 true true 195 105 240 150 225 180 165 105
Polygon -7500403 true true 120 90 60 150 75 180 135 105

person doctor
false
0
Circle -16777216 true false 181 1 30
Polygon -16777216 true false 107 92 62 197 92 212 116 158 122 197 92 272 212 272 182 197 188 157 212 212 242 197 197 92 167 92 152 152 137 92
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -13345367 true false 135 90 150 105 135 135 150 150 165 135 150 105 165 90
Polygon -7500403 true true 105 90 60 195 90 210 135 105
Polygon -7500403 true true 195 90 240 195 210 210 165 105
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -1 true false 105 90 60 195 90 210 114 156 120 195 90 270 210 270 180 195 186 155 210 210 240 195 195 90 165 90 150 150 135 90
Line -16777216 false 150 148 150 270
Line -16777216 false 196 90 151 149
Line -16777216 false 104 90 149 149
Circle -1 true false 180 0 30
Line -16777216 false 180 15 120 15
Line -16777216 false 150 195 165 195
Line -16777216 false 150 240 165 240
Line -16777216 false 150 150 165 150

person-1
false
0
Circle -7500403 true true 120 0 60
Polygon -7500403 true true 150 75 135 75 120 75 75 165 90 180 120 120 120 180 90 270 120 300 150 195 180 300 210 270 180 180 180 120 210 180 225 165 180 75

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="population 1000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-708" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <timeLimit steps="17"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-P-1000" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <timeLimit steps="17"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1st-hand quanratine-4d" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1st-hand quanratine-5d" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="incub-quanratine-test" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-incub" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR</go>
    <timeLimit steps="10"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="incub 5-quanratine-test" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="50"/>
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="incub 5-quanratine-test" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
      <value value="4000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="incub 5-quanratine-test" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
      <value value="4000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-incub" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR</go>
    <timeLimit steps="15"/>
    <metric>Cumulative-sick</metric>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="35" step="5" last="60"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-incub" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR</go>
    <metric>new-sick</metric>
    <metric>avg-growth-rate</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-TR-3" repetitions="15" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR</go>
    <metric>new-sick</metric>
    <metric>avg-growth-rate</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="quanratine test 1-4 levels" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="50" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="quanratine test 1-4 levels-1" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="50" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 test 1-4 levels-2" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SIR</go>
    <timeLimit steps="16"/>
    <metric>avg-growth-rate</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="75"/>
      <value value="80"/>
      <value value="85"/>
      <value value="90"/>
      <value value="95"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 1000 Quarantine test" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Asymptomatic test" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>avg-growth-rate</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;2nd-hand&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Asymptomatic-Carriers" first="0" step="10" last="60"/>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 2000 Quarantine test" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="2000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 3000 Quarantine test" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="3000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 7000 Quarantine test" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
      <value value="&quot;4th-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="7000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 2000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 3000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 4000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="4000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 5000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 6000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="6000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 7000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="7000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 8000" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 8000 Quarantine test" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 8000 Quarantine test-make up" repetitions="4" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-708-radius" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <timeLimit steps="17"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-incub-TR 6-10D" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <timeLimit steps="22"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-incub-TR 11" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <timeLimit steps="23"/>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 1000 Quarantine test-R3.7" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 1000 Quarantine test-R3.7-1" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="85" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="population 1000 Quarantine test-R3.7-2" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <metric>mean [cc] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="80"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Asymptomatic test" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;2nd-hand&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Asymptomatic-Carriers" first="5" step="10" last="55"/>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Asymptomatic test-fix QR" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>Mean [infect-#] of infected-with-symptom</metric>
    <metric>avg-growth-rate</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Asymptomatic-Carriers" first="0" step="10" last="90"/>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="R0 estimate test-709-1000" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR</go>
    <exitCondition>count turtles with [color = blue] &gt; 0</exitCondition>
    <metric>R0</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Transmission-rate" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="P 1000 R 2.19 Q test" repetitions="33" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>R0</metric>
    <metric>mean [#-contact] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="P 1000 R 2.46 Q test" repetitions="35" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>R0</metric>
    <metric>mean [#-contact] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Asymptomatic-Carriers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Contacts-Quarantined" first="0" step="5" last="100"/>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="P 1000 R 2.46 Q Asymp test" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-basic-SEIR-quarantine</go>
    <metric>Cumulative-sick * 100 / population-size</metric>
    <metric>cumul-level-quarantined-contacts</metric>
    <metric>R0</metric>
    <metric>mean [#-contact] of turtles</metric>
    <enumeratedValueSet variable="Levels-of-Contact">
      <value value="&quot;1st-hand&quot;"/>
      <value value="&quot;2nd-hand&quot;"/>
      <value value="&quot;3rd-hand&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="%-Asymptomatic-Carriers" first="0" step="5" last="90"/>
    <enumeratedValueSet variable="Quarantine-Time-lags">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Incubation-period">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mobility">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Patient-Groups">
      <value value="&quot;New Patients only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-Contacts-Quarantined">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Transmission-rate">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
