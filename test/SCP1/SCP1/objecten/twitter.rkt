#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{Twitter}
We willen een eenvoudige Twitter applicatie schrijven waarbij gebruikers een
account kunnen aanmaken en gebruikers elkaar kunnen volgen. Een account laat
uiteraard toe om @emph{tweets} te verzenden, die dan door al de  @emph{volgers}
kunnen gelezen worden.

@section{Tweet}
Ontwerp een ADT @emph{Tweet} dat je als object implementeert.
Een tweet onthoudt de gebruikersnaam van de persoon die het bericht getweet heeft, alsook de
mogelijks bijhorende tags. 
Wanneer het bericht uit meer dan 140 karakters bestaat, wordt er @scheme[#f] teruggegeven.
Een tweet moet minstens deze vier boodschappen
begrijpen:
@itemize{@item{De boodschap @scheme[username] geeft de gebruikersnaam van de
                            persoon die de tweet getweet heeft.}
          @item{De boodschap @scheme[text] geeft de tekst terug.}
          @item{De boodschap @scheme[tags] geeft de tags die bij de tweet horen terug.}
          @item{De boodschap @scheme[display] geeft de tweet weer op het scherm.}}

@solution{@defs+int[((define (display-all . args)
                       (for-each display args))
                     (define (display-all-sep args)
                       (for-each (lambda (arg) (display arg) (display " ")) 
                                 args))
                     
                     (define (make-tweet username text tags)
                       
                       (define (display-tweet)
                         (display-all "Tweet from " username "\n" text "\nTags: ")
                         (display-all-sep tags)
                         (newline))
                       
                       (define (dispatch msg)
                         (cond ((eq? msg 'text) text)
                               ((eq? msg 'tags) tags)
                               ((eq? msg 'username) username)
                               ((eq? msg 'display) display-tweet)
                               (else (display "error - wrong msg ") (display msg))))
                       
                       (if (> (string-length text) 140)
                           #f
                           dispatch)))]}

@interaction[(define my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))]
@interaction[(my-tweet 'username)]
@interaction[((my-tweet 'display))]


@section{Account}
Ontwerp een ADT account dat je als object implementeert. Een account onthoudt een
gebruikersnaam en de naam van de persoon die de account aanmaakt. Verder 
onthoudt een account ook zijn @emph{followers} (accounts van personen die hem 
volgen), de berichten die hij getweet heeft (@emph{tweets}) en de tweets van 
zowel hemzelf als alle accounts die hijzelf volgt (@emph{tweet-wall}). Een 
account moet minstens deze vier boodschappen begrijpen:
@itemize{@item{De boodschap @scheme[username] geeft de gebruikersnaam terug.}
          @item{De boodschap @scheme[name] geeft de naam terug.}
          @item{De boodschap @scheme[follow] waarbij deze account zichzelf registreert als volger van een andere account.}
          @item{De boodschap @scheme[add-follower] die een account toevoegt aan de volgers van deze account.}
          @item{De boodschap @scheme[tweet] die gegeven tekst en tags een @emph{tweet} aanmaakt en deze toevoegt aan de @emph{tweets} van deze account. Hierbij wordt deze tweet ook toegevoegd aan de @emph{tweet-wall} van de accounts die deze account volgen (de accounts die worden bijgehouden in @emph{followers}).}
          @item{De boodschap @scheme[add-tweet-to-wall] waarbij een @emph{tweet} wordt toegevoegd aan de @emph{tweet-wall}.}
          @item{De boodschap @scheme[display] die ofwel de followers, tweet-wall of de volledige account weergeeft op het scherm.}}


@solution{@defs+int[((define (make-account name username)
                       (let ((followers '())   ; followers  = accounts of people that follow you
                             (tweets '())      ; tweets     = tweets you tweeted
                             (tweet-wall '())) ; tweet-wall = tweets you tweeted and tweets of people you follow
                         
                         (define (follow account)
                           ((account 'add-follower) dispatch))
                         
                         (define (add-follower account)
                           (set! followers (cons account followers)))
                         
                         (define (tweet text . tags)
                           (let ((tweet-obj (make-tweet username text tags)))
                             (set! tweets (cons tweet-obj tweets))
                             (set! tweet-wall (cons tweet-obj tweet-wall))
                             (for-each (lambda (follower) 
                                         ((follower 'add-tweet-to-wall) tweet-obj))
                                       followers)))
                         
                         (define (add-tweet-to-wall tweet)
                           (set! tweet-wall (cons tweet tweet-wall)))
                         
                         (define (display-account symbol)
                           (cond ((eq? symbol 'wall) (display-wall))
                                 ((eq? symbol 'followers) (display-followers))
                                 ((eq? symbol 'account) (display-entire-account))
                                 (else (display "wrong symbol given"))))
                         
                         (define (display-wall)
                           (display "TWEET WALL") (newline)
                           (for-each (lambda (tweet) ((tweet 'display)) (newline))
                                     tweet-wall))
                         
                         (define (display-followers)
                           (display "FOLLOWERS") (newline)
                           (for-each (lambda (follower) 
                                       (display (follower 'username)) (display " "))
                                     followers))
                         
                         (define (display-entire-account)
                           (display-all "Twitter name " username "\n"
                                        "Name " name "\n")
                           (display-wall)
                           (display-followers)
                           (newline) (newline))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'name)                           name)
                                 ((eq? msg 'username)                   username)
                                 ((eq? msg 'display)             display-account)
                                 ((eq? msg 'follow)                       follow)
                                 ((eq? msg 'add-follower)           add-follower)
                                 ((eq? msg 'tweet)                         tweet)
                                 ((eq? msg 'add-tweet-to-wall) add-tweet-to-wall)
                                 (else (display "error - wrong msg ") (display msg))))
                         dispatch)))]}

@interaction[(define accountE (make-account "Eline Philips" "ephilips"))]
@interaction[(define accountM (make-account "Mattias De Wael" "madewael"))]
@interaction[((accountE 'follow) accountM)]
@interaction[((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")]
@interaction[((accountE 'tweet) "Hello World!")]
@interaction[((accountE 'display) 'account)]
@interaction[((accountM 'display) 'account)]