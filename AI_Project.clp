;----------------------------------------------------------------------------
; CLASSES
;----------------------------------------------------------------------------
(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot occupation)
	(slot personality))

(defclass career
	(is-a USER)
	(slot occupation)
	(slot quality)
	(slot suggested_career))

;----------------------------------------------------------------------------
; DEFAULT INSTANCES
;----------------------------------------------------------------------------

(definstances PERSON-INSTANCES
	(Need of PERSON))

(definstances interest-INSTANCES
	(which_interest of career))

;----------------------------------------------------------------------------
; INITIAL USER INPUTS AND VALIDATIONS
;----------------------------------------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
; RULE TO GET THE USER INPUT
(defrule GetCompanion(declare (salience 10))
    =>
    (printout t crlf)
    (printout t crlf)    
    (send [Need] put-personality
    (user-input-validation "What type of mindset you have? (technical/social/creative):  "technical social creative)))
   
;----------------------------------------------------------------------------
; RULES OF THE EXPERT SYSTEM TO SELECT THE interest
;----------------------------------------------------------------------------

(defrule Techy_mindset
	?ins <- (object (is-a PERSON) (personality technical))
	=> 
	(printout t crlf)
   	(send [which_interest] put-quality
    (user-input-validation "What you love to do? (solve_cube/join_circuits/designing/make_cars): "solve_cube join_circuits designing make_cars)))
   	 

(defrule social_mindset
	?ins <- (object (is-a PERSON) (personality social))
	=> 
	(printout t crlf)
    (send [which_interest] put-quality
  	(user-input-validation "Tell me about yourself ? (confident_speaker/can_manipulate_people/understand_market/handle_people): "
	  		confident_speaker can_manipulate_people understand_market handle_people )))
   	 

(defrule creative_Mindy
	?ins <- (object (is-a PERSON) (personality creative))
	=> 
	(printout t crlf)
	(send [which_interest] put-quality
    (user-input-validation "Enter your type? (funny/confident/writer): "funny confident writer))))
   	 

(defrule confi_speaker_
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality social)))
	=> 
	(printout t crlf)
	(send [which_interest] put-occupation
    (user-input-validation "Enter your choice what you think, you are? (goodlooking):  "
        goodlooking)))


(defrule can_manipulate
	(and ?ins <- (object (is-a career))
	(object (is-a PERSON)(personality social)))
	=> 
	(printout t crlf)
	(send [which_interest] put-occupation
    (user-input-validation "Enter your preferred choice (handle_situtation):  "
        handle_situtation)))

(defrule market_
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality social)))
	=> 
	(printout t crlf)
	(send [which_interest] put-occupation
    (user-input-validation "Enter your preferred choice (stock_price):  "
        stock_price)))


(defrule handle_the_people_
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality social)))
	=> 
	(printout t crlf)
	(send [which_interest] put-occupation
    (user-input-validation "Enter your preferred choice (time_management_skilled):  "
        time_management_skilled)))

   	

(defrule funny_a_lot
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality creative)))
	=> 
	(printout t crlf)
   	(send [which_interest] put-occupation
   	(user-input-validation "Enter your preferred choice (good_humour):  "
          good_humour)))

    
(defrule confident_
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality creative)))
	=> 
	(printout t crlf)
   	(send [which_interest] put-occupation
   	(user-input-validation "Enter your preferred choice (actor):  "
         actor)))

(defrule writer_
	(and ?ins <- (object (is-a career) )
	(object (is-a PERSON)(personality creative)))
	=> 
	(printout t crlf)
   	(send [which_interest] put-occupation
   	(user-input-validation "Enter your preferred choice (socialist):  "
         socialist)))

(defrule solve_cube_
	(and ?ins <- (object (is-a PERSON) (personality technical))
	(object (is-a career) (quality solve_cube)))
	=> 
	(printout t crlf)
   	(send [which_interest] put-occupation
   	(user-input-validation "What you prefer? (spying/complex_calculation/creative_work): "spying complex_calculation creative_work))))


(defrule join_circuits_
	(and ?ins <- (object (is-a PERSON) (personality technical))
	(object (is-a career) (quality join_circuits)))
	=> 
	(printout t crlf)
   	(send [which_interest] put-occupation
   	(user-input-validation "What is your preferred choice? (programming/safeJob):  "
         programming safeJob )))
   	 

(defrule designing_
	(and ?ins <- (object (is-a PERSON) (personality technical))
	(object (is-a career) (quality designing)))
	=> 
	(printout t crlf)
  	(send [which_interest] put-occupation
   	(user-input-validation "What is your preferred choice? (build_home):  "
         build_home)))



; #Career


(defrule solve_cube_career
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation spying))
	(object (is-a career) (quality solve_cube)))
	=> 
	(send [which_interest] put-suggested_career "Ethical Hacker"))


(defrule solve_cube_car
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation complex_calculation))
	(object (is-a career) (quality solve_cube)))
	=> 
	(send [which_interest] put-suggested_career "Software Developer"))


(defrule sol_cub
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation creative_work))
	(object (is-a career) (quality solve_cube)))
	=> 
	(send [which_interest] put-suggested_career "Web Developer"))


(defrule circuits
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation programming))
	(object (is-a career) (quality join_circuits)))
	=> 
	(send [which_interest] put-suggested_career "Robotics field"))
	

(defrule jo_cir
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation safeJob))
	(object (is-a career) (quality join_circuits)))
	=> 
	(send [which_interest] put-suggested_career "GATE"))
	


(defrule design
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation build_home))
	(object (is-a career) (quality designing)))
	=> 
	(send [which_interest] put-suggested_career "Architecture"))
	


(defrule making
	(and ?ins <- (object (is-a PERSON) (personality technical) (occupation LoveCars))
	(object (is-a career) (quality make_cars)))
	=> 
	(send [which_interest] put-suggested_career "Mechanical Engineer"))
	
	
	
(defrule confi_speak
	(and ?ins <- (object (is-a career) (quality confident_speaker) (occupation goodlooking))
	(object(is-a PERSON)(personality social)))
	=> 
	(send ?ins put-suggested_career "News Anchor") )


(defrule manipulate_people
	(and ?ins <- (object (is-a PERSON) (personality social))
	(object (is-a career) (occupation handle_situtation)(quality can_manipulate_people)))
	=> 
	(send [which_interest] put-suggested_career "Minister"))
	

(defrule understandingofmarket
	(and ?ins <- (object (is-a PERSON) (personality social))
	(object (is-a career) (occupation stock_price)(quality understand_market)))
	=> 
	(send [which_interest] put-suggested_career "BuisnessMan"))
	

(defrule handle_peopl
	(and ?ins <- (object (is-a PERSON) (personality social))
	(object (is-a career) (occupation time_management_skilled)(quality handle_people)))
	=> 
	(send [which_interest] put-suggested_career "Manager"))
	

(defrule funnnny
	(and ?ins <- (object (is-a career) (quality funny)(occupation good_humour))
	(object (is-a PERSON) (personality creative)))
	=> 
	(send ?ins put-suggested_career "Stand_up_comedian"))
	

(defrule happy
	(and ?ins <- (object (is-a career) (quality confident)(occupation actor))
	(object (is-a PERSON) (personality creative)))
	=> 
	(send ?ins put-suggested_career "Artist"))
	

(defrule writer__
	(and ?ins <- (object (is-a career) (quality writer)(occupation socialist))
	(object (is-a PERSON) (personality creative)))
	=> 
	(send ?ins put-suggested_career "Journalist"))
	
	
;  FINAL SUGGESSION	


(defrule choose_Career (declare (salience -1))
	(object (is-a career) (suggested_career ?mov))
	=>
	(printout t crlf)
	(printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf)
    (printout t "The recommended Career which best suits your Personality is: " ?mov crlf)
    (printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf))