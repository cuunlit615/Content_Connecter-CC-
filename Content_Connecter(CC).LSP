;;; ===========================================================================
;;; 指令：CC (屬性同步 - 垂直線自動轉 CCC 模式)
;;; ===========================================================================

(vl-load-com)

;; --- 全域變數預設 ---
(if (not *CC_tol*)  (setq *CC_tol* 0.5)) 
(if (not *CC_Tags*) (setq *CC_Tags* '("M00" "22/2" "PHNX")))

;; --- 基礎屬性讀寫函式 (VLA 模式) ---
(defun CC_GetAtt (en tag / obj result done)
  (if (null en) (setq result nil)
    (progn
      (setq obj (vlax-ename->vla-object en))
      (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
               (= :vlax-true (vla-get-HasAttributes obj)))
          (foreach a (vlax-invoke obj 'GetAttributes)
            (if (and (null done) (= (strcase (vla-get-TagString a)) (strcase tag)))
                (setq result (vla-get-TextString a) done t))))))
  (if result (vl-string-trim " " result) nil))

(defun CC_SetAtt (en tag val / obj done)
  (if (and en val)
    (progn
      (setq obj (vlax-ename->vla-object en))
      (foreach a (vlax-invoke obj 'GetAttributes)
        (if (and (null done) (= (strcase (vla-get-TagString a)) (strcase tag)))
            (progn (vla-put-TextString a val) (setq done t))))
      (vla-update obj) done)))

;; --- 主指令：CC ---
(defun c:CC (/ ent ename p1 p2 p1a p1b p2a p2b ss1 ss2 b1 b2 sblk dblk tags v loop x1 x2 pick vals target_loop err)
  (setq tags *CC_Tags* loop t)
  (princ "\n--- CC: 屬性同步 (左源右標 / 垂直轉 CCC 模式) ---")

  (while loop
    (setvar "ERRNO" 0)
    (setq ent (entsel "\n選取連接線段 [按 Enter 結束]: "))
    (cond
      ((and ent (setq ename (car ent)) 
            (member (cdr (assoc 0 (entget ename))) '("LWPOLYLINE" "POLYLINE" "LINE")))
       
       (setq p1 (vlax-curve-getStartPoint ename)
             p2 (vlax-curve-getEndPoint ename))

       ;; 建立端點偵測區域 (建立 ss1, ss2 僅用於判斷 X 座標)
       (setq p1a (mapcar '- p1 (list *CC_tol* *CC_tol* 0))
             p1b (mapcar '+ p1 (list *CC_tol* *CC_tol* 0))
             p2a (mapcar '- p2 (list *CC_tol* *CC_tol* 0))
             p2b (mapcar '+ p2 (list *CC_tol* *CC_tol* 0)))

       (setq ss1 (ssget "_C" p1a p1b '((0 . "INSERT")))
             ss2 (ssget "_C" p2a p2b '((0 . "INSERT"))))

       (if (and ss1 ss2)
           (progn
             (setq b1 (ssname ss1 0)
                   b2 (ssname ss2 0)
                   x1 (car (cdr (assoc 10 (entget b1))))
                   x2 (car (cdr (assoc 10 (entget b2)))))

             (cond
               ;; --- 狀況 A: 非垂直線 (自動偵測左源右標) ---
               ((not (equal x1 x2 0.001))
                (if (< x1 x2) (setq sblk b1 dblk b2) (setq sblk b2 dblk b1))
                (foreach tg tags
                  (setq v (CC_GetAtt sblk tg))
                  (if v (CC_SetAtt dblk tg v)))
                (princ "\n>> 自動同步成功 (左至右)。"))

               ;; --- 狀況 B: 垂直線 (直接轉 CCC 手動選取模式) ---
               (t 
                (princ "\n[垂直線偵測] X 相同，請改用手動選取：")
                (setq sblk nil)
                ;; 步驟 1: 選取來源 (完全自由點選，不受限於端點)
                (while (null sblk)
                  (setq pick (entsel "\n   -> [CCC模式] 選取「來源」圖塊: "))
                  (if (and pick (setq sblk (car pick)))
                      (if (/= (cdr (assoc 0 (entget sblk))) "INSERT") 
                          (progn (princ "\n[提示] 請選取一個圖塊。") (setq sblk nil)))
                      (if (= (getvar "ERRNO") 52) (setq sblk 'exit))))
                
                (if (and sblk (/= sblk 'exit))
                    (progn
                      ;; 讀取來源資料
                      (setq vals nil)
                      (foreach tg tags (setq vals (append vals (list (CC_GetAtt sblk tg)))))
                      
                      ;; 步驟 2: 選取目標 (完全自由點選)
                      (princ "\n   >> 來源資料已鎖定，請選取「目標」圖塊。")
                      (setq target_loop t)
                      (while target_loop
                        (setvar "ERRNO" 0)
                        (setq pick (entsel "\n   -> [CCC模式] 選取「目標」圖塊: "))
                        (setq err (getvar "ERRNO"))
                        (cond
                          ((and pick (setq dblk (car pick)))
                           (if (= (cdr (assoc 0 (entget dblk))) "INSERT")
                               (progn
                                 (mapcar '(lambda (tg vv) (if vv (CC_SetAtt dblk tg vv))) tags vals)
                                 (princ "\n>> 同步成功。")
                                 (setq target_loop nil))
                               (princ "\n[提示] 選取的物件不是圖塊，請重新選取目標。")))
                          ((= err 52) 
                           (princ "\n>> 放棄本次垂直同步。")
                           (setq target_loop nil))
                          (t (princ "\n[提示] 未選到目標，請點選目標圖塊（來源資料仍鎖定中）..."))
                        )
                      )
                    )
                )
               )
             )
           )
           (princ "\n[錯誤] 線段端點偵測失敗，請確認圖塊在端點或調整容差。")
       )
      )
      ((= (getvar "ERRNO") 52) (setq loop nil)) 
    )
  )
  (princ "\n已退出 CC。")
  (princ)
)

(defun c:CC_SET (/ ct i inp tags)
  (prompt "\n=== CC_SET：同步參數設定 ===")
  (setq ct (getreal (strcat "\n端點偵測誤差容許值 <" (rtos *CC_tol* 2 2) ">: ")))
  (if ct (setq *CC_tol* ct))
  (setq i 0 tags '())
  (while (< i 3)
    (setq inp (getstring T (strcat "\n同步 TAG " (itoa (1+ i)) " <" (nth i *CC_Tags*) ">: ")))
    (setq tags (append tags (list (if (= inp "") (nth i *CC_Tags*) inp))))
    (setq i (1+ i)))
  (setq *CC_Tags* tags) (princ))

(princ "\n[載入成功] CC (垂直轉手動選取模式) | CC_SET")
(princ)