;;; magnatune-helm --- a helm interface to magnatune's music catalog

;; Copyright © 2014 Eike Kettner

;; Version: 0.5.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides a helm interface to magnatune's music catalog

;; todo:
;;  - record played items and provide a "recent" source
;;  - make the album buffer nicer by displaying images (?)
;;  - play a selection of items

;;; Code:

(require 'magnatune)
(require 'helm)
(require 'dash)

(defvar magnatune-helm--use-cache t
  "For development it is sometimes useful to disable the source
  cache.")

(defmacro magnatune-helm--if-item (item &rest body)
  (declare (indent 1))
  (let ((type (gensym)))
    `(let ((,type (magnatune-item-type ,item)))
       (cond
        ((eq ,type 'artist)
         ,(plist-get body :artist))
        ((eq ,type 'album)
         ,(plist-get body :album))
        ((eq ,type 'song)
         ,(plist-get body :song))
        ((eq ,type 'genre)
         ,(plist-get body :genre))
        (t (error "Unknown item: %s" ,type))))))

(defun magnatune-helm-play-action (item)
  "Plays all songs of ITEM."
  (if current-prefix-arg
      (message "Play %s clearing list…" (magnatune-item-type item))
    (message "Enqueuing %s …" (magnatune-item-type item)))
  (let ((urls
         (magnatune-helm--if-item item
           :artist (magnatune-artist-stream-urls (plist-get item :artists_id))
           :album (magnatune-album-stream-urls (plist-get item :album_id))
           :song (-list (magnatune-make-stream-url item))
           :genre (message "Not playing complete genre,"))))
    (magnatune--run-url-hooks urls current-prefix-arg)))

(defun magnatune-helm-play ()
  (interactive)
  (with-helm-alive-p
    (helm-execute-persistent-action 'play-action)))

(defun magnatune-helm-description-action (item)
  (interactive)
  (let ((buffer (magnatune-helm--if-item item
                  :artist (magnatune--make-artist-buffer)
                  :album (magnatune--make-album-buffer)
                  :song (magnatune--make-album-buffer)
                  :genre (user-error "No details about a genre."))))
    (with-current-buffer buffer
      (if (eq 'song (magnatune-item-type item))
          (setq magnatune--query `(:album_id ,(plist-get item :album_id)))
        (setq magnatune--query item))
      (magnatune-browse-update-view))
    (switch-to-buffer buffer)))

(defun magnatune-helm-description ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     'magnatune-helm-description-action)))

(defun magnatune-helm-navigate-next-action (item)
  (magnatune-helm--if-item item
    :artist (magnatune-helm-album-list item)
    :album (magnatune-helm-song-list item)
    :genre (magnatune-helm-album-list item)
    :song (message "Cannot go further down.")))

(defun magnatune-helm-navigate-next ()
  (interactive)
  (with-helm-alive-p
    (let ((sname (assoc-default 'name (helm-get-current-source))))
      (unless (string-prefix-p "Songs" sname)
        (helm-quit-and-execute-action
         'magnatune-helm-navigate-next-action)))))

(defun magnatune-helm-navigate-back-action (item)
  (magnatune-helm--if-item item
    :artist (message "Artist is top-level already")
    :album (magnatune-helm)
    :song (magnatune-helm-album-list
           (list :name (plist-get item :artist)
                 :artists_id (plist-get item :artists_id)))
    :genre (message "Genre is top-level already")))

(defun magnatune-helm-navigate-back ()
  (interactive)
  (with-helm-alive-p
    (let ((sname (assoc-default 'name (helm-get-current-source))))
      (unless (string-prefix-p "All" sname)
        (helm-quit-and-execute-action
         'magnatune-helm-navigate-back-action)))))

(defun magnatune-helm--page-action (item urlfn)
  (let ((url (funcall urlfn item)))
    (if helm-current-prefix-arg
        (browse-url url)
      (progn
          (kill-new url)
          (message "Copied url %s" url)))))

(defun magnatune-helm-artist-page-action (item)
  "Copy the url to the artists web page at magnatune. With a
prefix arg, visit the page using `browse-url'. The same
information is displayed in a buffer using the default persistent
action."
  (magnatune-helm--page-action item 'magnatune-get-artist-page-url))

(defun magnatune-helm-album-page-action (item)
  "Copy the url to the album web page at magnatune. With a prefix
arg, visit the page using `browse-url'. The same information is
displayed in a buffer using the default persistent action."
  (magnatune-helm--page-action item 'magnatune-get-album-page-url))

(defun magnatune-helm-artist-page ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     'magnatune-helm-artist-page-action)))

(defun magnatune-helm-album-page ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     'magnatune-helm-album-page-action)))

(defun magnatune-helm-copy-stream-urls-action (item)
  "Copies all stream urls of the given ITEM to the kill
ring. With prefix argument, use the free urls even if a
membership is configured."
  (let ((urls
         (magnatune-helm--if-item item
           :artist (magnatune-artist-stream-urls (plist-get item :artists_id)
                                                 helm-current-prefix-arg)
           :album (magnatune-album-stream-urls (plist-get item :album_id)
                                               helm-current-prefix-arg)
           :song (-list (magnatune-make-stream-url item
                                                   helm-current-prefix-arg))
           :genre (user-error "Cannot copy all streams of a genre."))))
    (if urls
        (progn
          (kill-new (mapconcat 'identity urls "\n"))
          (message "Copied stream urls to kill ring."))
      (message "No stream urls found."))))

(defun magnatune-helm-copy-stream-urls ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     'magnatune-helm-copy-stream-urls-action)))

(defun magnatune-helm-download-album-action (item)
  "Download the album."
  (magnatune-helm--if-item item
    :artist (message "Not on an album.")
    :album (magnatune-download-album (plist-get item :sku))
    :song (message "Not on an album.")
    :genre (message "Not on an album.")))

(defun magnatune-helm-download-album ()
  (interactive)
  (with-helm-alive-p
    (helm-execute-persistent-action 'download-action)))

(defclass magnatune-helm-source (helm-source-sync)
  ((play-action
    :initarg :play-action
    :initform #'magnatune-helm-play-action
    :custom function
    :documentation "The peristent play action.")
   (download-action
    :initarg :download-action
    :initform #'magnatune-helm-download-album-action
    :custom function
    :documentation "The peristent download action.")))

(defmacro magnatune-helm-build-source (name &rest args)
  "Build a synchronous helm source with name NAME.
Args ARGS are keywords provided by `helm-source-sync'."
  (declare (indent 1))
  `(helm-make-source ,name 'magnatune-helm-source ,@args))

(defun magnatune-helm--prepare-artist-candidate (artist)
  (let ((infos (format "(%d albums, %d tracks, %smin)"
                       (plist-get artist :albums)
                       (plist-get artist :songs)
                       (magnatune--format-time (plist-get artist :length)))))
    (put-text-property 0 (length infos)
                       'font-lock-face font-lock-comment-face infos)
    (cons (format "%s – %s %s"
                  (plist-get artist :name)
                  (plist-get artist :description)
                  infos)
          artist)))

(defvar magnatune-helm--artists-sources nil)
(defun magnatune-helm-all-artists-source ()
  (unless (and magnatune-helm--artists-sources magnatune-helm--use-cache)
    (setq magnatune-helm--artists-sources
          (magnatune-helm-build-source "All Magnatune artists"
            :candidates (-map #'magnatune-helm--prepare-artist-candidate
                              (magnatune-search-artist ""))
            :action `(("Browse albums" . magnatune-helm-navigate-next-action)
                      ("Play" . magnatune-helm-play-action)
                      ("Description" . magnatune-helm-description-action)
                      ("Copy artist web page, C-u visit" . magnatune-helm-artist-page-action)
                      ("Copy all stream urls, C-u free ones" . magnatune-helm-copy-stream-urls-action))
            :persistent-action 'magnatune-helm-description-action)))
  magnatune-helm--artists-sources)

(defun magnatune-helm--prepare-album-candidate (album)
  (let* ((release (plist-get album :release_date))
         (pop (plist-get album :popularity))
         (songs (plist-get album :songs))
         (genres (mapconcat 'identity (plist-get album :genres) ", "))
         (len (magnatune--format-time (plist-get album :length)))
         (infos (format "(%s, %s: %d tracks, %smin%s)"
                        (format-time-string "%Y" (seconds-to-time release))
                        genres songs len
                        (if (> pop 0)
                            (format ", %d☻" pop)
                          ""))))
    (put-text-property 0 (length infos)
                       'font-lock-face font-lock-comment-face infos)
    (cons (format "%s – %s %s"
                  (plist-get album :artist)
                  (plist-get album :name)
                  infos)
          album)))

(defun magnatune-helm-make-album-source (item)
  "Create a helm source for all albums of ITEM which is either an
artist or a genre."
  (magnatune-helm-build-source (format "%s's albums"
                                       (plist-get item :name))
    :candidates (magnatune-helm--if-item item
                  :artist (-map #'magnatune-helm--prepare-album-candidate
                                (magnatune-list-albums item))
                  :genre (-map #'magnatune-helm--prepare-album-candidate
                               (magnatune-search-albums item)))
    :action `(("Browse songs" . magnatune-helm-navigate-next-action)
              ("Play" . magnatune-helm-play-action)
              ("Back" . magnatune-helm-navigate-back-action)
              ("Description" . magnatune-helm-description-action)
              ("Copy artist web page, C-u visit" . magnatune-helm-artist-page-action)
              ("Copy album web page, C-u visit" . magnatune-helm-album-page-action)
              ("Copy all stream urls, C-u free ones" . magnatune-helm-copy-stream-urls-action)
              ("Download album" . magnatune-helm-download-album-action))
    :persistent-action 'magnatune-helm-description-action))

(defvar magnatune-helm--albums-sources nil)
(defun magnatune-helm-all-albums-source ()
  (unless (and magnatune-helm--albums-sources magnatune-helm--use-cache)
    (message "Please wait for the album list to build …")
    (setq magnatune-helm--albums-sources
          (magnatune-helm-build-source "All Magnatune albums"
            :candidates (-map #'magnatune-helm--prepare-album-candidate
                              (magnatune-search-albums ""))
            :action `(("Browse songs" . magnatune-helm-navigate-next-action)
                      ("Play" . magnatune-helm-play-action)
                      ("Description" . magnatune-helm-description-action)
                      ("Copy artist web page, C-u visit" . magnatune-helm-artist-page-action)
                      ("Copy album web page, C-u visit" . magnatune-helm-album-page-action)
                      ("Copy all stream urls, C-u free ones" . magnatune-helm-copy-stream-urls-action)
                      ("Download album" . magnatune-helm-download-album-action))
            :persistent-action 'magnatune-helm-description-action)))
  magnatune-helm--albums-sources)

(defun magnatune-helm--prepare-song-candidate (song)
  (let ((time (format "(%s)"
                      (magnatune--format-time (plist-get song :duration)))))
    (put-text-property 0 (length time)
                       'font-lock-face font-lock-comment-face time)
    (cons (format "%02d – %s %s"
                  (plist-get song :track_no)
                  (plist-get song :name)
                  time)
          song)))

(defun magnatune-helm-make-songs-source (album)
  (magnatune-helm-build-source (format "Songs on %s"
                                       (plist-get album :name))
    :candidates (-map #'magnatune-helm--prepare-song-candidate
                      (magnatune-list-songs album))
    :action `(("Play" . magnatune-helm-play-action)
              ("Back" . magnatune-helm-navigate-back-action)
              ("Description" . magnatune-helm-description-action)
              ("Copy artist web page, C-u visit" . magnatune-helm-artist-page-action)
              ("Copy album web page, C-u visit" . magnatune-helm-album-page-action)
              ("Copy stream url, C-u free one" . magnatune-helm-copy-stream-urls-action))
    :persistent-action 'magnatune-helm-description-action))

(defun magnatune-helm--prepare-genre-candidate (genre)
  (let ((info (format "(%s albums)" (plist-get genre :albums))))
    (put-text-property 0 (length info)
                       'font-lock-face font-lock-comment-face info)
    (cons (format "%s %s"
                  (plist-get genre :name)
                  info)
          genre)))

(defvar magnatune-helm--genres-sources nil)
(defun magnatune-helm-all-genres-source ()
  (unless (and magnatune-helm--genres-sources magnatune-helm--use-cache)
    (setq magnatune-helm--genres-sources
          (magnatune-helm-build-source "All Genres"
            :candidates (-map #'magnatune-helm--prepare-genre-candidate
                              (magnatune-list-genres))
            :action '(("Browse albums" . magnatune-helm-navigate-next-action))
            :persistent-action 'identity)))
  magnatune-helm--genres-sources)


(defun magnatune-helm-song-list (album)
  "helm for all songs of a given album"
  (helm :sources (magnatune-helm-make-songs-source album)
        :buffer "*helm album songs*"
        :keymap magnatune-helm-keymap
        :prompt "Song: "))

(defun magnatune-helm-album-list (artist-or-genre)
  "helm for all albums of a given artist or genre"
  (helm :sources (magnatune-helm-make-album-source artist-or-genre)
        :buffer "*helm albums*"
        :keymap magnatune-helm-keymap
        :prompt "Album: "))

(defvar magnatune-helm-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") 'magnatune-helm-description)
    (define-key map (kbd "C-a") 'magnatune-helm-play)
    (define-key map (kbd "C-e") 'magnatune-helm-navigate-next)
    (define-key map (kbd "C-.") 'magnatune-helm-navigate-back)
    (define-key map (kbd "C-b a") 'magnatune-helm-artist-page)
    (define-key map (kbd "C-b l") 'magnatune-helm-album-page)
    (define-key map (kbd "C-b s") 'magnatune-helm-copy-stream-urls)
    (define-key map (kbd "C-d") 'magnatune-helm-download-album)
    map))

;;;###autoload
(defun magnatune-helm ()
  "Helm for all genres, albums and artists at magnatune.

On artist, album or song the persistent action opens a buffer
with a description of the current item (same buffer as with
standard interface). When on a song, a description of the album
of this song is shown.

The “play” action is bound to 'C-a'. It appends the tracks of the
current item to the playlist. With prefix, the playlist is
cleared first. It is a persistent action, so you can add to the
playlist without quitting the helm session.

The default action steps into the current item and you can go
back with 'C-.'."
  (interactive)
  (helm :sources (list (magnatune-helm-all-genres-source)
                       (magnatune-helm-all-artists-source)
                       (magnatune-helm-all-albums-source))
        :buffer "*helm magnatune*"
        :keymap magnatune-helm-keymap
        :prompt "Choose: "))

(provide 'magnatune-helm)
