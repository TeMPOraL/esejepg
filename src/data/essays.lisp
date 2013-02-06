;;; translation staff

;; Some default stuff
(defconstant +default-properties+ '(:title nil :url nil :orig-title nil :orig-url nil :date nil :orig-date nil :alt-translations nil :translators nil :editors nil :disabled nil :additional-html nil :part-of-hnp nil :description ""))

;;; Data processing code
;;; Used for being able to refer to essays declaratively.
(defun defessay (essay-id &rest properties)
  "Defines an essay by putting it's data into global *environment* variable, which will be an environment for
EMB templates. Please define essays from oldest to newest, to ensure proper order when iterating (from newest to
oldest."
  (append (list :id essay-id)
		  properties
		  (list :template (concatenate 'string "src/eseje/" essay-id ".html"))
		  +default-properties+))

(defun create-translators (&rest translators)
  (map 'list #'(lambda (person) (list :translator person)) translators))

(defun create-editors (&rest editors)
  (map 'list #'(lambda (person) (list :editor person)) editors))


(defparameter *essays*
  (let (( +temporal+ "Jacek \"TeMPOraL\" Złydach")
		( +aajnno+ "Joanna Kmiecik"))

;;; essays
	(list 
	 (defessay "progbot"
		 :title "Programowanie metodą wstępującą (ang. bottom-up)"
		 :url "/eseje/progbot.html"
		 :orig-title "Programming Bottom-Up"
		 :orig-url "http://paulgraham.com/progbot.html"
		 :date "Styczeń 2013"
		 :orig-date "1993"
		 :translators (create-translators +temporal+)
		 :editors (create-editors +aajnno+)
		 :description "O całkowicie odmiennym spojrzeniu na projektowanie i pisanie oprogramowania, jakie oferuje nam Lisp.")

	 ;; Chapter 2 of Ansi Common Lisp

	 ;; Chapter 1 of Ansi Common Lisp

	 ;; Lisp for Web-Based Applications

	 ;; Beating the Averages

	 ;; Java`s Cover

	 ;; Being Popular

	 ;; Five Questions about Language Design

	 ;; The Roots of Lisp

	 ;; The Other Road Ahead

	 ;; What Made Lisp Different

	 ;; Why Arc Isn`t Especially Object-Oriented

	 ;; Taste for Makers

	 (defessay "fix"
		 :title "Co rozwiązują języki"
		 :url "/eseje/fix.html"
		 :orig-title "What Languages Fix"
		 :orig-url "http://paulgraham.com/fix.html"
		 :date "Styczeń 2013"
		 :translators (create-translators +temporal+)
		 :editors (create-editors +aajnno+)
		 :description "Tylko częściowo humorystyczne, jednozdaniowe podsumowania znanych języków programowania.")

	 ;; Succinctness is Power

	 ;; Revenge of the Nerds

	 ;; A Plan for Spam

	 ;; Design and Research

	 ;; Better Bayesian Filtering

	 ;; Why Nerds are Unpopular

	 ;; The Hundred-Year Language

	 ;; If Lisp is So Great

	 ;; Hackers and Painters

	 ;; Filters that Fight Back

	 ;; What You Can`t Say

	 ;; The Word "Hacker"

	 ;; How to Make Wealth

	 ;; Mind the Gap

	 ;; Great Hackers

	 ;; The Python Paradox

	 ;; The Age of the Essay

	 ;; What the Bubble Got Right

	 ;; A Version 1.0

	 ;; Bradley`s Ghost

	 ;; It`s Charisma, Stupid

	 ;; Made in USA

	 (defessay "hs"
		 :title "Co będziecie chcieli, żeby ktoś był powiedział Wam wcześniej"
		 :url "/eseje/hs.html"
		 :date "Styczeń 2013"
		 :orig-title "What You`ll Wish You`d Known"
		 :orig-url "http://paulgraham.com/hs.html"
		 :orig-date "Styczeń 2005"
										;    :alt-translations `(("http://T1" "T1") ("http://T2" "T2") ("http://T3" "T3"))
		 :translators (create-translators +aajnno+)
		 :editors (create-editors +temporal+)
		 :description "Przemówienie dla licealistów na temat tego, co czeka ich w dorosłym życiu i jak już teraz mogą zacząć wykorzystywać swój czas najlepiej.")

	 ;; How to Start a Startup

	 ;; A Unified Theory of VC Suckage

	 ;; Undergraduation

	 (defessay "writing44"
		 :title "O pisaniu, zwięźle"
		 :url "/eseje/writing44.html"
		 :date "Grudzień 2012"
		 :orig-title "Writing, Briefly"
		 :orig-url "http://paulgraham.com/writing44.html"
		 :orig-date "Marzec 2005"
		 :translators (create-translators +aajnno+ +temporal+)
		 :description "Wysoce skondensowany poradnik pisania esejów. Esencja pisarstwa w kilku zdaniach.")

	 ;; Return of the Mac

	 ;; Why Smart People Have Bad Ideas

	 ;; The Submarine

	 ;; Hiring is Obsolete

	 ;; What Business Can Learn from Open Source

	 ;; After the Ladder

	 ;; Inequality and Risk

	 ;; What I Did this Summer

	 ;; Ideas for Startups

	 ;; The Venture Capital Squeeze

	 ;; How to Fund a Startup

	 ;; Web 2.0

	 (defessay "procrastination"
		 :title "Dobra i zła prokrastynacja"
		 :url "/eseje/procrastination.html"
		 :date "Styczeń 2013"
		 :orig-title "Good and Bad Procrastination"
		 :orig-url "http://paulgraham.com/procrastination.html"
		 :orig-date "Grudzień 2005"
		 :translators (create-translators +aajnno+)
		 :editors (create-editors +temporal+)
		 :description "Czy każda prokrastynacja jest zła? Jak odwlekać zadania i pozostać produktywnym?")

	 ;; How to Do What You Love

	 ;; Why YC

	 ;; 6,631,372

	 ;; Are Software Patents Evil?

	 ;; See Randomness

	 ;; The Hardest Lessons for Startups to Learn

	 ;; How to Be Silicon Valley

	 ;; Why Startups Condense in America

	 ;; The Power of the Marginal

	 ;; The Island Test

	 ;; Copy What You Like

	 ;; How to Present to Investors

	 ;; A Student`s Guide to Startups

	 ;; The 18 Mistakes That Kill Startups

	 ;; How Art Can Be Good

	 ;; Learning from Founders

	 ;; Is It Worth Being Wise?

	 ;; Why to Not Not Start a Startup

	 ;; Microsoft is Dead

	 (defessay "judgement"
		 :title "Dwa rodzaje osądów"
		 :url "/eseje/judgement.html"
		 :date "Styczeń 2013"
		 :orig-title "Two Kinds of Judgement"
		 :orig-url "http://paulgraham.com/judgement.html"
		 :orig-date "Kwiecien 2007"
		 :translators (create-translators +aajnno+)
		 :editors (create-editors +temporal+)
		 :description "Dlaczego nie każdą ocenę należy odbierać osobiście, i jak odróżnić od siebie dwa rodzaje osądu?")

	 ;; The Hacker`s Guide to Investors

	 ;; An Alternative Theory of Unions

	 ;; The Equity Equation

	 ;; Stuff

	 ;; Holding a Program in One`s Head

	 ;; How Not to Die

	 ;; News from the Front

	 ;; How to Do Philosophy

	 ;; The Future of Web Startups

	 ;; Why to Move to a Startup Hub

	 ;; Six Principles for Making New Things

	 ;; Trolls

	 ;; A New Venture Animal

	 ;; You Weren`t Meant to Have a Boss

	 ;; How to Disagree

	 ;; Some Heroes

	 ;; Why There Aren`t More Googles

	 ;; Be Good

	 ;; Lies We Tell Kids

	 ;; Disconnecting Distraction

	 ;; Cities and Ambition

	 ;; The Pooled-Risk Company Management Company

	 ;; A Fundraising Survival Guide

	 ;; Why to Start a Startup in a Bad Economy

	 ;; The Other Half of "Artists Ship"  

	 ;; The High-Res Society

	 ;; Could VC be a Casualty of the Recession?

	 ;; After Credentials

	 ;; Keep Your Identity Small  

	 ;; Startups in 13 Sentences

	 ;; What I`ve Learned from Hacker News

	 ;; Can You Buy a Silicon Valley? Maybe.

	 ;; Why TV Lost

	 ;; How to Be an Angel Investor

	 ;; Relentlessly Resourceful

	 ;; Five Founders

	 ;; The Founder Visa

	 ;; A Local Revolution?

	 ;; Maker`s Schedule, Manager`s Schedule 
	 (defessay "makersschedule"
		 :title "Harmonogram twórcy, harmonogram menadżera"
		 :url "/eseje/makersschedule.html"
		 :date "Styczeń 2013"
		 :orig-title "Maker`s Schedule, Manager`s Schedule"
		 :orig-url "http://paulgraham.com/makersschedule.html"
		 :orig-date "Lipiec 2009"
		 :translators  (create-translators +aajnno+)
		 :editors (create-editors +temporal+)
		 :description "Dlaczego programiści i menadżerowie tak często nie potrafią dogadać się ze sobą?")

	 ;; Ramen Profitable

	 ;; The Trouble with the Segway

	 ;; What Kate Saw in Silicon Valley  

	 ;; The Anatomy of Determination 

	 ;; The List of N Things

	 ;; Post-Medium Publishing

	 ;; Persuade xor Discover 

	 ;; What Startups Are Really Like

	 ;; Apple`s Mistake

	 ;; Organic Startup Ideas

	 ;; How to Lose Time and Money 

	 ;; The Top Idea in Your Mind 

	 ;; The Acceleration of Addictiveness

	 ;; The Future of Startup Funding 

	 ;; What Happened to Yahoo 

	 ;; High Resolution Fundraising 

	 ;; Where to See Silicon Valley

	 ;; The New Funding Landscape

	 ;; What We Look for in Founders

	 ;; Tablets

	 ;; Founder Control

	 ;; Subject: Airbnb

	 ;; The Patent Pledge

	 ;; Why Startup Hubs Work

	 ;; Snapshot: Viaweb, June 1998

	 ;; Schlep Blindness

	 ;; A Word to the Resourceful

	 ;; Frighteningly Ambitious Startup Ideas

	 ;; Defining Property

	 ;; Writing and Speaking

	 (defessay "todo"
		 :title "Szczyt mojej listy rzeczy do zrobienia"
		 :url "/eseje/todo.html"
		 :date "Styczeń 2013"
		 :orig-title "The Top of My Todo List"
		 :orig-url "http://paulgraham.com/todo.html"
		 :orig-date "Kwiecień 2012"
		 :translators (create-translators +aajnno+)
		 :editors (create-editors +temporal+)
		 :description "O czym warto pamiętać cały czas, niezależnie co się w danej chwili robi.")

	 ;; Black Swan Farming

	 ;; Startup = Growth

	 (defessay "hw"
		 :title "Renesans Sprzętu"
		 :url "/eseje/hw.html"
		 :date "Styczeń 2013"
		 :orig-title "The Hardware Renaissance"
		 :orig-url "http://paulgraham.com/hw.html"
		 :orig-date "Październik 2012"
		 :translators (create-translators +temporal+)
		 :editors (create-editors +aajnno+)
		 :description "Coraz więcej startupów zaczyna zajmować się sprzętem. Czy jest to oznaka nadchodzącej zmiany?")

	 ;; How to Get Startup Ideas

	 )))

(defparameter *essays-ids* (mapcar #'(lambda (essay) (getf essay :id)) *essays*))

(defun find-essay-by-id (id)
  (find-if #'(lambda (essay) (string= id (getf essay :id))) *essays*))
