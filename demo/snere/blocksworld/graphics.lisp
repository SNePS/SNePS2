;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPSUL; Base: 10 -*-

;; Copyright (C) 1993--2013
;; Research Foundation of State University of New York

;; Version: $Id: graphics.lisp,v 1.2 2013/08/28 19:07:23 shapiro Exp $

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;;; The contents of this file are subject to the University at
;;; Buffalo Public License Version 1.0 (the "License"); you may
;;; not use this file except in compliance with the License. You
;;; may obtain a copy of the License at 
;;; http://www.cse.buffalo. edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an
;;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;;; or implied. See the License for the specific language gov
;;; erning rights and limitations under the License.
;;; 
;;; The Original Code is SNePS 2.8.
;;; 
;;; The Initial Developer of the Original Code is Research Foun
;;; dation of State University of New York, on behalf of Univer
;;; sity at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C)
;;; 2011 Research Foundation of State University of New York, on
;;; behalf of University at Buffalo. All Rights Reserved.


;; $END LICENSE$





;;; Altered for ACL 6.1 by FLJ  a->|A|, b->|B|, c->|C|, d->|D|

(in-package :snepsul)


; Start the background process that will help refresh the window
(opal:kill-main-event-loop-process)
(opal:launch-main-event-loop-process)
;
; The blocksworld window....
(kr:create-instance 'blockswindow inter:interactor-window
		 (:left 10) (:top 10)
                 (:width 1000) (:height 400)
		 (:title "Blocksworld")
;		 (:background-color lavender)
		 (:icon-title "Blocksworld"))
;
; All objects in the window will be a part of the BLOCKSWORLD aggregate
;
(kr:create-instance 'blocksworld opal:aggregate)
;
(kr:s-value blockswindow :aggregate blocksworld)
;
(defparameter bigfnt (kr:create-instance nil opal:font
				      (:size :very-large)
			   	      (:family :serif)))
; Read the bit map of the robot arm
;
(opal:add-component blocksworld
		    (kr:create-instance 'arm opal:bitmap
				     (:left 50) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;arm.bm"))))
;
; This is a big fat ugly arm for John!
(opal:add-component blocksworld
		    (kr:create-instance 'john-arm opal:bitmap
				     (:left -150) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;arm-john.bm"))))
;(opal:add-component blocksworld
;		    (kr:create-instance 'john-text opal:text
;				     (:fast-redraw-p t)
;				     (:line-style opal:default-line-style)
;				     (:draw-function :xor)
;				     (:font bigfnt) (:string "John")))
;(kr:s-value john-text :left (kr:o-formula (lisp:+ (kr:gv john-arm :left) 18)))
;(kr:s-value john-text :top (kr:o-formula (lisp:+ (kr:gv john-arm :top) 15)))
;
; The various eye positions for simulating eye-movements...gimmick
(opal:add-component blocksworld
		    (kr:create-instance 'r-eye opal:bitmap
				     (:left -250) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;eye-r.bm"))))

(opal:add-component blocksworld
		    (kr:create-instance 'c-eye opal:bitmap
				     (:left -250) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;eye-c.bm"))))

(opal:add-component blocksworld
		    (kr:create-instance 'l-eye opal:bitmap
				     (:left -250) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;eye-l.bm"))))

(opal:add-component blocksworld
		    (kr:create-instance 'up-eye opal:bitmap
				     (:left -250) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;eye-up.bm"))))

(opal:add-component blocksworld
		    (kr:create-instance 'lo-eye opal:bitmap
				     (:left -250) (:top 30)
;				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;eye-lo.bm"))))


(opal:add-component blocksworld
		    (kr:create-instance 'signature opal:bitmap
				     (:left 930) (:top 300)
				     (:fast-redraw-p t) (:draw-function :xor)
				     (:image (opal:read-image "sneps:demo;snere;blocksworld;dk.bm"))))
;
; Create the left grasper of the arm (the part that slides and grasps)
;
(opal:add-component blocksworld
		    (kr:create-instance 'arm-stand opal:rectangle
				     (:left 0) (:top 5)
				     (:width 1000) (:height 20)
			;	     (:fast-redraw-p t) (:draw-function :xor)
				     (:filling-style opal:orange-fill)))
;
; Part of the robot arm
 (opal:add-component blocksworld
		      (kr:create-instance 'line-1 opal:line
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:x1 400) (:y1 10)
				       (:x2 400) (:y2 30)))
 (opal:add-component blocksworld
		      (kr:create-instance 'line-2 opal:line
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:x1 430) (:y1 10)
				       (:x2 430) (:y2 30)))
(kr:s-value line-1 :x2 (kr:o-formula (lisp:+ 20 (kr:gv arm :left))))
(kr:s-value line-1 :y2 (kr:o-formula (kr:gv arm :top)))
(kr:s-value line-1 :x1 (kr:o-formula (lisp:+ 20 (kr:gv arm :left))))
(kr:s-value line-2 :x2 (kr:o-formula (lisp:+ 60 (kr:gv arm :left))))
(kr:s-value line-2 :y2 (kr:o-formula (kr:gv arm :top)))
(kr:s-value line-2 :x1 (kr:o-formula (lisp:+ 60 (kr:gv arm :left))))
;
(opal:add-component blocksworld
		    (kr:create-instance 'left-grasper opal:rectangle
				     (:left 0) (:top 0)
				     (:width 10) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				     (:filling-style opal:purple-fill)))
;
; Create the right grasper of the arm (the part that slides and grasps)
;
(opal:add-component blocksworld
		    (kr:create-instance 'right-grasper opal:rectangle
				     (:left 0) (:top 0)
				     (:width 10) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				     (:filling-style opal:purple-fill)))
;
; This slot governs the positioning of the graspers (up or down)
;
(kr:s-value arm :grasping nil)
;
; Hook up the graspers to the arm
;
(kr:s-value left-grasper :left (kr:o-formula (lisp:+ 4 (kr:gv arm :left))))
(kr:s-value right-grasper :left (kr:o-formula (lisp:+ 66 (kr:gv arm :left))))
(kr:s-value left-grasper :top (kr:o-formula
			    (if (kr:gv arm :grasping)
				(lisp:+ 80 (kr:gv arm :top))
				(lisp:+ 60 (kr:gv arm :top)))))
(kr:s-value right-grasper :top (kr:o-formula
			     (if (kr:gv arm :grasping)
				 (lisp:+ 80 (kr:gv arm :top))
				 (lisp:+ 60 (kr:gv arm :top)))))
;
; create the table
;
 (opal:add-component blocksworld 
		      (kr:create-instance 'table opal:rectangle
				       (:left 50) (:top 300)
				       (:width 900) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:filling-style opal:light-gray-fill)))
(opal:add-component blocksworld 
		      (kr:create-instance 'leg1 opal:rectangle
				       (:left 125) (:top 350)
				       (:width 25) (:height 50)
				       (:filling-style opal:light-gray-fill)))
(opal:add-component blocksworld 
		      (kr:create-instance 'leg2 opal:rectangle
				       (:left 850) (:top 350)
				       (:width 25) (:height 50)
				       (:filling-style opal:light-gray-fill)))

;
; create the blocks
;
(opal:add-component blocksworld 
		    (kr:create-instance '|A| opal:rectangle
				       (:left 200) (:top 250)
				       (:width 50) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:filling-style opal:red-fill)
				     ))
(opal:add-component blocksworld
		    (kr:create-instance 'a-text opal:text
				     (:fast-redraw-p t)
				     (:line-style opal:default-line-style)
				     (:draw-function :xor)
				     (:font bigfnt) (:string "A")))
(kr:s-value a-text :left (kr:o-formula (lisp:+ (kr:gv |A| :left) 18)))
(kr:s-value a-text :top (kr:o-formula (lisp:+ (kr:gv |A| :top) 15)))

(opal:add-component blocksworld 
		      (kr:create-instance '|B| opal:rectangle
				       (:left 450) (:top 250)
				       (:width 50) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:filling-style opal:blue-fill)
				     ))
(opal:add-component blocksworld
		    (kr:create-instance 'b-text opal:text				     (:fast-redraw-p t) (:draw-function :xor)
				     (:font bigfnt) (:string "B")))
(kr:s-value b-text :left (kr:o-formula (lisp:+ (kr:gv |B| :left) 18)))
(kr:s-value b-text :top (kr:o-formula (lisp:+ (kr:gv |B| :top) 15)))

(opal:add-component blocksworld 
		    (kr:create-instance '|C| opal:rectangle
					(:left 700) (:top 250)
					(:width 50) (:height 50)
					(:fast-redraw-p t) (:draw-function :xor)
					(:filling-style opal:red-fill)
					))
(opal:add-component blocksworld
		    (kr:create-instance 'c-text opal:text
				     (:fast-redraw-p t) (:draw-function :xor)
				     (:font bigfnt) (:string "C")))
(kr:s-value c-text :left (kr:o-formula (lisp:+ (kr:gv |C| :left) 18)))
(kr:s-value c-text :top (kr:o-formula (lisp:+ (kr:gv |C| :top) 15)))
(opal:add-component blocksworld 
		      (kr:create-instance '|D| opal:rectangle
				       (:left -150) (:top 150)
				       (:width 50) (:height 50)
				     (:fast-redraw-p t) (:draw-function :xor)
				       (:filling-style opal:blue-fill)
				     ))
(opal:add-component blocksworld
		    (kr:create-instance 'd-text opal:text
				     (:fast-redraw-p t) (:draw-function :xor)
				     (:font bigfnt) (:string "D")))
(kr:s-value d-text :left (kr:o-formula (lisp:+ (kr:gv |D| :left) 18)))
(kr:s-value d-text :top (kr:o-formula (lisp:+ (kr:gv |D| :top) 15)))

(kr:s-value arm :home-top 30)
(kr:s-value arm :home-left 50)
(kr:s-value john-arm :home-left 900)
(kr:s-value john-arm :home-top 50)
(kr:s-value |A| :home-left 200)
(kr:s-value |A| :home-top 250)
(kr:s-value |B| :home-left 450)
(kr:s-value |B| :home-top 250)
(kr:s-value |C| :home-left 700)
(kr:s-value |C| :home-top 250)
(kr:s-value |D| :home-left 100)
(kr:s-value |D| :home-top 250)


(defun bw-initialize ()
  ;; Re/initializes and re/displays the blocksworld window.
  ;; This is ugly, since all positions/sizes etc. are hardcoded which
  ;; makes the size of the window unchangeable.  Everything should be
  ;; defined via constraints relative to the size of the blocksworld
  ;; window.  The use of fixed-size bitmaps complicates things.
  ;; Maybe later (too much work to do this right now), hc 4/3/95.
  (kr:s-value arm :left 50)
  (kr:s-value arm :top 30)
  (kr:s-value arm :grasping nil)
  (kr:s-value arm :grasped-block nil)
  (kr:s-value john-arm :left -150)
  (kr:s-value john-arm :top 30)
  (kr:s-value r-eye :left -150)
  (kr:s-value r-eye :top 30)
  (kr:s-value c-eye :left -150)
  (kr:s-value c-eye :top 30)
  (kr:s-value l-eye :left -150)
  (kr:s-value l-eye :top 30)
  (kr:s-value up-eye :left -150)
  (kr:s-value up-eye :top 30)
  (kr:s-value lo-eye :left -150)
  (kr:s-value lo-eye :top 30)
  (kr:destroy-constraint |A| :left)
  (kr:destroy-constraint |A| :top)
  (kr:s-value |A| :left 200)
  (kr:s-value |A| :top 250)
  (kr:destroy-constraint |B| :left)
  (kr:destroy-constraint |B| :top)
  (kr:s-value |B| :left 450)
  (kr:s-value |B| :top 250)
  (kr:destroy-constraint |C| :left)
  (kr:destroy-constraint |C| :top)
  (kr:s-value |C| :left 700)
  (kr:s-value |C| :top 250)
  (kr:destroy-constraint |D| :left)
  (kr:destroy-constraint |D| :top)
  (kr:s-value |D| :left -150)
  (kr:s-value |D| :top 250)
  (opal:update blockswindow)
  (opal:deiconify-window blockswindow))

;
; animation functions
;
(defun translate-x (arm distance dir)
    (dotimes (n (floor distance 5) t)
      (kr:s-value arm :left (lisp:+ (lisp:* 5 dir)
			    (kr:g-value arm :left)))
      (opal:update blockswindow)
;      (sleep sleep-time)
      ))
;
;
 (defun translate-y (arm distance dir)
    (dotimes (n (floor distance 5) t)
      (kr:s-value arm :top (lisp:+ (lisp:* 5 dir)
			    (kr:g-value arm :top)))
      (opal:update blockswindow)
;      (sleep sleep-time)
      ))
;
(defun move-arm (arm newx newy)     ; newx=newleft newy=newtop
    (let* ((oldx (kr:g-value arm :left))
	   (oldy (kr:g-value arm :top))
	   (x-distance (lisp:- newx oldx))
	   (y-distance (lisp:- newy oldy))
	   (x-dir (if (minusp x-distance) -1 1))
	   (y-dir (if (minusp y-distance) -1 1)))
      (cond ((minusp y-dir)
	     (translate-y arm (abs y-distance) y-dir)
	     (kr:s-value arm :top newy)
	     (translate-x arm (abs x-distance) x-dir)
	     (kr:s-value arm :left newx))
	    (t (translate-x arm (abs x-distance) x-dir)
	       (kr:s-value arm :left newx)
	       (translate-y arm (abs y-distance) y-dir)
	       (kr:s-value arm :top newy)))
      (opal:update blockswindow)))
;
(defun grasp ()
    (kr:s-value arm :grasping t)
    (opal:update blockswindow))
;
(defun ungrasp ()
    (kr:s-value arm :grasping nil)
    (opal:update blockswindow))
;

(defun bw-putdown (block object)
    (let ((new-left (if (equal object table)
			(kr:g-value block :home-left)
			(kr:g-value object :left)))
	  (new-top (if (equal object table)
		       (kr:g-value block :home-top)
		       (lisp:- (kr:g-value object :top) 50))))
      (move-arm arm (lisp:- new-left 15) (lisp:- new-top 101))
      (ungrasp)
      (kr:s-value block :left (kr:o-formula new-left))
      (kr:s-value block :top (kr:o-formula new-top))
      (move-arm arm (kr:g-value arm :left) (kr:g-value arm :home-top))))
;
(defun bw-pickup (block)
    (move-arm arm (lisp:- (kr:g-value block :left) 15)
	          (lisp:- (kr:g-value block :top) 101))
    (kr:s-value block :left  (kr:o-formula (lisp:+ (kr:gv arm :left) 15)))
    (kr:s-value block :top (kr:o-formula (lisp:+ (kr:gv arm :top) 101)))
    (kr:s-value arm :grasping t)
    (kr:s-value arm :grasped-block block)
    (opal:update blockswindow)
    (move-arm arm (kr:g-value arm :left) (kr:g-value arm :home-top)))
;



; this causes the eye to make an elaborated deliberate floating-eye movement
(defun bw-scan (object)
  "Animated scanning of the object!"
  (kr:s-value lo-eye :left
	      (lisp:- (kr:g-value object :left) 50))
  (kr:s-value lo-eye :top 30)
  (translate-y lo-eye (lisp:- (kr:g-value object :top) 30) 1)
  (kr:s-value l-eye :left (kr:g-value lo-eye :left))
  (kr:s-value l-eye :top (kr:g-value lo-eye :top))
  (kr:s-value lo-eye :left -150)
  (opal:update blockswindow)
  (sleep 0.3)
  (kr:s-value up-eye :left (kr:g-value l-eye :left))
  (kr:s-value up-eye :top (kr:g-value l-eye :top))
  (kr:s-value l-eye :left -150)
  (opal:update blockswindow)
  (sleep 0.3)
  (kr:s-value r-eye :left (kr:g-value up-eye :left))
  (kr:s-value r-eye :top (kr:g-value up-eye :top))
  (kr:s-value up-eye :left -150)
  (opal:update blockswindow)
  (sleep 0.3)
  (kr:s-value c-eye :left (kr:g-value r-eye :left))
  (kr:s-value c-eye :top (kr:g-value r-eye :top))
  (kr:s-value r-eye :left -150)
  (opal:update blockswindow)
  (dotimes (n 3 t)
    (eye-scan))
  (kr:s-value lo-eye :left (kr:g-value c-eye :left))
  (kr:s-value lo-eye :top (kr:g-value c-eye :top))
  (kr:s-value c-eye :left -150)
  (opal:update blockswindow)
  (sleep 0.3)
  (kr:s-value l-eye :left (kr:g-value lo-eye :left))
  (kr:s-value l-eye :top (kr:g-value lo-eye :top))
  (kr:s-value lo-eye :left -150)
  (opal:update blockswindow)
  (sleep 0.3)
  (kr:s-value up-eye :left (kr:g-value l-eye :left))
  (kr:s-value up-eye :top (kr:g-value l-eye :top))
  (kr:s-value l-eye :left -150)
  (opal:update blockswindow)
  (sleep 1)
  (translate-y up-eye (lisp:- (kr:g-value up-eye :top) -50) -1))

; Finds the color of the object and return it
(defun bw-find-color (object)
  "Returns the color of the graphics object."
  (bw-scan object)
  (format nil "~(~A~)" (kr:name-for-schema
			(kr:g-value
			 (kr:g-value object :filling-style)
			 :foreground-color))))
;
; simulated agent arm to pick up a block
(defun bw-john-pickup (block)
  (move-arm john-arm
	    (lisp:- (kr:g-value block :left) 15)
	    (lisp:- (kr:g-value block :top) 75))
  (kr:s-value block :left (kr:o-formula (lisp:+ (kr:gv john-arm :left) 15)))
  (kr:s-value block :top (kr:o-formula (lisp:+ (kr:gv john-arm :top) 75)))
  (kr:s-value john-arm :grasping t)
  (kr:s-value john-arm :grasped-block block)
  (opal:update blockswindow)
  (move-arm john-arm (kr:g-value john-arm :home-left)
	    (kr:g-value john-arm :home-top)))

(defun bw-john-putdown (block support)
  (let ((new-left (if (equal support table)
                      (kr:g-value block :home-left)
                    (kr:g-value support :left)))
        (new-top (if (equal support table)
                     (kr:g-value block :home-top)
                   (lisp:- (kr:g-value support :top) 50))))
    (move-arm john-arm (lisp:- new-left 15) (lisp:- new-top 75))
    (kr:s-value john-arm :grasping nil)
    (opal:update blockswindow)
    (kr:s-value block :left (kr:o-formula new-left))
    (kr:s-value block :top (kr:o-formula new-top))
    (move-arm john-arm (kr:g-value john-arm :home-left)
              (kr:g-value john-arm :home-top))))

;   causes the eye to flip from center to right
(defun eye-scan ()
  (kr:s-value r-eye :left (kr:g-value c-eye :left))
  (kr:s-value r-eye :top (kr:g-value c-eye :top))
  (kr:s-value c-eye :left -250)
  (opal:update blockswindow)
  (sleep 1)
  (kr:s-value c-eye :left (kr:g-value r-eye :left))
  (kr:s-value c-eye :top (kr:g-value r-eye :top))
  (kr:s-value r-eye :left -250)
  (opal:update blockswindow)
  (sleep 1))



    
    




