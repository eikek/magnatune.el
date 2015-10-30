;;; magnatune.el --- browse magnatune's music catalog

;; Copyright © 2014 Eike Kettner

;; Version: 0.5.1

;; Package-Requires: ((dash "2.9.0") (s "1.9.0"))

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

;; This is a interface to the music catalog on magnatune.com. It
;; allows to browse and search the catalog and take actions on an
;; album or song, like playing it with emms or mpc. It's for those who
;; like to enjoy magnatune and emacs at the same time!

;; It needs the sqlite3 command to work. For more info, please see
;; https://github.com/eikek/magnatune.el.

;; There is a helm interface to the catalog provided via `(require
;; 'magnatune-helm)'. Obviously helm must be installed (it is not
;; pulled in automatically).

;;; Code:

(require 'thingatpt)
(require 'dash)
(require 's)
(require 'mm-decode)

(defvar magnatune-username nil
  "The username used for payed membership access")

(defvar magnatune-password nil
  "The password used for payed membership access")

(defvar magnatune-streaming-format "ogg"
  "The file format used for streaming. Default is ogg.")

(defvar magnatune-download-format "flac"
  "The format used when downloading albums.")

(defvar magnatune-free-url-fmt "http://he3.magnatune.com/all/%s.%s"
  "The url pattern for streaming a song.")

(defvar magnatune-member-url-fmt
  "http://%s:%s@download.magnatune.com/all/%s_nospeech.%s"
  "The url pattern used for streaming songs with membership access.")

(defvar magnatune-download-url-fmt
  "http://download.magnatune.com/membership/download3?sku=%s&format=%s"
  "The url used to donwload albums.")

(defvar magnatune-download-folder
  (expand-file-name "Downloads" "~")
  "Folder used to store donwloaded albums.")

(defvar magnatune-download-hook nil
  "Hook executed with downloaded album file.")

(defvar magnatune-sqlite-download-url
  "http://he3.magnatune.com/info/sqlite_normalized.db.gz"
  "Magnatunes catalog as sqlite file.")

(defvar magnatune-enable-gzip t
  "Whether or not to download the .gz version of the database
file and uncompress it locally using gzip.")

(defvar magnatune-changes-url "http://magnatune.com/info/changed.txt"
  "Magnatunes changed file.")

(defvar magnatune-workdir (expand-file-name "magnatune"
                                            (expand-file-name ".emacs.d" "~"))
  "The working directory to store the db file.")

(defvar magnatune-sqlite-cmd "/usr/bin/sqlite3"
  "The sqlite3 executable.")

(defvar magnatune-artist-page-url-fmt "http://magnatune.com/artists/%s"
  "The url pattern to an artist page.")

(defvar magnatune-album-page-url-fmt "http://magnatune.com/artists/albums/%s"
  "The url pattern to an album page.")

(defvar magnatune-cover-url-fmt "http://he3.magnatune.com/music/%s/%s/cover_%d.jpg"
  "The url pattern to a album cover image.
It is 'http://he3.magnatune.com/music/urlenc(artist)/urlenc(album)/cover_100.jpg'")

(defvar magnatune-cover-size 300
  "The size of the cover image.
Magnatune states to support the following:
50, 75, 100, 160, 200, 300, 600, 1400")

(defvar magnatune-band-photo-url-fmt "http://he3.magnatune.com"
  "The url pattern for retrieving band photos.")

(defvar magnatune-log-sqlite-output nil
  "Whether to log all sqlite output. By default this is only logged on error.")

(defvar magnatune-external-player "mplayer"
  "The external player executable.")

(defvar magnatune-open-urls-hook nil
  "Hook functions called with a list of stream urls and the
optional prefix arg in `magnatune-open-item-at-point'.

There are two functions provided:

- `magnatune-mpc-url-hook' this will add the urls to the local
  mpd using emacs built in mpc library. With prefix arg, the
  current playlist is cleared first.

- `magnatune-external-url-hook' This will call
  `magnatune-external-player' with the urls.")

(defvar magnatune-browse-move-hook nil
  "Hook executed after moved to a new item.")

(defvar magnatune-column-width 80
  "Maxium columns used for displaying things.")

(defvar magnatune--update-fn)
(defvar magnatune--query)
(defvar magnatune--offset)
(defvar magnatune--limit)
(defvar magnatune--order)
(defvar magnatune--results)
(defvar magnatune--truncated-items)


(defun magnatune--make-url (url-fmt &rest args)
  "Creates a complete url out of URL-FMT placing ARGS into it."
  (let ((params (-map (lambda (arg)
                        (if (stringp arg)
                            (url-hexify-string arg)
                          arg))
                      args)))
    (apply 'format url-fmt params)))

(defun magnatune--init-workdir ()
  "Create the `magnatune-workdir' directory."
  (unless (file-exists-p magnatune-workdir)
    (make-directory magnatune-workdir t)))


(defun magnatune--sqlite-convert-results (buf)
  "Convert results from a sqlite query into a plist.

The sqlite3 command must be invoked with option '-line'. This
does not work with multi-line values!"
  (with-current-buffer buf
    (goto-char (point-min))
    (insert "'((")
    (while (< (point) (point-max))
      (if (looking-at "^$")
          (progn (insert ")")
                 (beginning-of-line 2)
                 (insert "("))
        (when (looking-at "[ \t]+\\|\n")
          (forward-whitespace 1))
        (if (looking-at "[a-zA-Z0-9\\-]+")
            (insert ":")
          (error "Wrong format (%d): %s" (point) (buffer-string)))
        (search-forward "=" nil t)
        (backward-delete-char 1)
        (while (looking-at "[ \t]")
          (delete-char 1))
        (cond
         ((looking-at "[ \t]*$")
          (insert "nil"))
         ((looking-at "[0-9]+$")
          (goto-char (line-end-position)))
         (t
          (insert "\"")
          (while (re-search-forward "\"" (line-end-position) t)
            (replace-match "" nil nil))
          (goto-char (line-end-position))
          (insert "\"")))
        (beginning-of-line 2)))
    (insert "))")
    (goto-char (point-min))
    (if (looking-at "'(())")
        nil
      (eval (read buf)))))

(defmacro magnatune-with-sqlite-buffer (cmd &rest body)
  "Execute sqlite3 CMD against magnatune db.

If CMD returns successfully, BODY is evaluated with the current
buffer set to a temporary buffer that contains the output of
CMD. Additionally CMD is logged to a '*sqlite-log*' buffer and if
CMD failed, the output is logged, too. Otherwise output is not
logged, but this may be overriden by settings
`magnatune-log-sqlite-output' to t."
  `(let ((db (magnatune-get-catalog))
        (sqlite magnatune-sqlite-cmd)
        (cmds (mapconcat 'identity (-list ,cmd) "; "))
        (log  (get-buffer-create "*sqlite-log*")))
     (with-current-buffer log
       (newline)
       (insert "> Execute: " cmds)
       (newline))
     (with-temp-buffer
       (let ((rc (call-process sqlite nil t nil "-line" db cmds))
             (out (current-buffer)))
         (with-current-buffer log
           (insert "  "
                   (number-to-string rc)
                   (if (eq rc 0) " success." " failed."))
           (newline)
           (when (or magnatune-log-sqlite-output (/= 0 rc))
             (insert "--- output ---")
             (newline)
             (insert-buffer-substring out)
             (newline)
             (insert "--- end output ---")
             (newline)))
         (if (eq rc 0)
             (progn
               ,@body)
           (error "Error return from sqlite command: %d" rc))))))

(put 'magnatune-with-sqlite-buffer 'lisp-indent-function 1)

(defun magnatune--sqlite-select (query)
  "Execute a sqlite3 query and return the results as a plist.

FILE can be a database file, or the default in
`org-expenses/sqlite-db-file' is used. The results are converted into
a plist where the key is the column name converted into a
keyword. Thus, it is necessary to have column names that make up
valid keywords (the colon is prepended and should not be part of
the column name). Note: this does not work with multiline values."
  (magnatune-with-sqlite-buffer query
    (magnatune--sqlite-convert-results (current-buffer))))


(defun magnatune--catalog-changed-p ()
  "Returns t if the db file is stale."
  (let* ((timefile (expand-file-name "db.time" magnatune-workdir))
         (changefile (expand-file-name "changed.txt" magnatune-workdir))
         (checksum (when (file-exists-p changefile)
                     (with-temp-buffer
                       (insert-file-contents changefile)
                       (buffer-string))))
         (currms (string-to-number (format-time-string "%s")))
         (lastms (when (file-exists-p timefile)
                   (with-temp-buffer
                     (insert-file-contents timefile)
                     (string-to-number (buffer-string)))))
         (idle  (* 7 24 60 60)))
    (when (or (null lastms)
              (null checksum)
              (< (+ lastms idle) currms))
      (let ((temp (make-temp-file "magnatune-changes")))
        (magnatune--init-workdir)
        (url-copy-file magnatune-changes-url temp t)
        (let ((str (with-temp-buffer
                     (insert-file-contents temp)
                     (buffer-string))))
          (with-temp-file changefile
            (insert str))
          (with-temp-file timefile
            (insert (int-to-string currms)))
          (not (equal str checksum)))))))

(defun magnatune-get-catalog ()
  "Download magnatunes catalog file.
This is an sqlite db file which is downloaded as a gzipped file
and stored locally. The gunzip tool is then used to unpack the
file. The file is downloaded if new contents are detected. Return
the path to the db file."
  (interactive)
  (let* ((localfile (if magnatune-enable-gzip
                        "magnatune.db.gz"
                      "magnatune.db"))
         (dbfilegz (expand-file-name localfile magnatune-workdir))
         (dbfile (if magnatune-enable-gzip
                     (file-name-sans-extension dbfilegz)
                   dbfilegz)))
    (when (or (magnatune--catalog-changed-p)
              (not (file-exists-p dbfile)))
      (message "Downloading magnatune's catalog…")
      (magnatune--init-workdir)
      (url-copy-file (if magnatune-enable-gzip
                         magnatune-sqlite-download-url
                       (file-name-sans-extension magnatune-sqlite-download-url))
                     dbfilegz t)
      (when magnatune-enable-gzip
        (message "Unzipping file…")
        (with-temp-buffer
          (let ((rc (call-process "gzip" nil t nil "-df" dbfilegz)))
            (when (/= rc 0)
              (error "Could not gzip -df the magnatune db file: %s"
                     (buffer-string)))))))
    dbfile))

(defun magnatune-item-type (item)
  "Return the type of ITEM.
This is either 'song, 'album, 'artist or 'genre."
  (cond
   ((plist-get item :genre_id) 'genre)
   ((plist-get item :song_id) 'song)
   ((plist-get item :album_id) 'album)
   ((plist-get item :artists_id) 'artist)))

(defun magnatune-item-id (item)
  "Return the id of ITEM."
  (or
   (plist-get item :genre_id)
   (plist-get item :song_id)
   (plist-get item :album_id)
   (plist-get item :artists_id)))

(defun magnatune--null-or-blank-p (str)
  (or (null str)
      (and (stringp str) (s-blank? str))))

(defun magnatune-search-artist (&optional query-or-id strict offset limit)
  "Search artists using QUERY-OR-ID.
If STRICT is t, QUERY-OR-ID must match exactly. Otherwise search
artists starting with QUERY-OR-ID. If QUERY-OR-ID is a number, it
is used to lookup the artist by this id. The result is a list of
plists."
  (let ((select (concat "select ar.artists_id, ar.name,ar.description,ar.homepage,ar.photo,"
                        "  count(distinct al.album_id) as albums, count(s.song_id) as songs, "
                        "  sum(s.duration) as length "
                        "from artists ar, albums al, songs s "
                        "where ar.artists_id = al.artist_id and s.album_id = al.album_id %s "
                        "group by (ar.artists_id)"
                        "order by ar.name collate nocase"
                        (if (and offset limit)
                            (format " limit %d,%d" offset limit)
                          ""))))
    (cond
     ((magnatune--null-or-blank-p query-or-id)
      (magnatune--sqlite-select (format select "")))
     ((numberp query-or-id)
      (magnatune--sqlite-select
       (format select (format "and ar.artists_id = %d" query-or-id))))
     ((and (stringp query-or-id) strict)
      (magnatune--sqlite-select
       (format select (format "and ar.name = '%s'" query-or-id))))
     ((and (stringp query-or-id) (not strict))
      (magnatune--sqlite-select
       (format select (format "and ar.name like '%s'" query-or-id))))
     (t (user-error "Unrecognised query: %s" (prin1 query-or-id))))))

(defun magnatune-get-artist-info (id)
  "Return the bio of the artist with ID."
  (let ((select (format "select bio from artists where artists_id = %d" id)))
    (magnatune-with-sqlite-buffer select
      (goto-char (point-min))
      (search-forward "bio = ")
      (delete-region (point-min) (point))
      (s-trim (buffer-string)))))


(defun magnatune--album-query (&optional where offset limit order)
  "Query the database for albums conforming to WHERE.
Also query the genre tables to add a list of genres to each album."
  (let* ((orderby (cond
                   ((eq order 'name) "al.name collate nocase")
                   ((eq order 'popularity) "al.popularity desc")
                   (t "al.release_date desc")))
         (select (concat "select ar.artists_id, ar.name as artist, al.album_id, "
                         "  al.name, al.sku, al.upc, al.release_date, al.popularity, "
                         "  ar.homepage as artist_page, ar.photo as artist_photo, "
                         "  count(distinct s.song_id) as songs, sum(s.duration) / "
                         "  count(distinct g.genre_id) as length "
                         " from artists ar, albums al, songs s, genres_albums ga, genres g "
                         "where ar.artists_id = al.artist_id and s.album_id = al.album_id "
                         " and ga.album_id = al.album_id and g.genre_id = ga.genre_id %s "
                         "group by (al.album_id) "
                         "order by " orderby
                         (if (and offset limit)
                             (format " limit %d,%d" offset limit)
                           "")))
         (albums (if (null where)
                     (magnatune--sqlite-select (format select ""))
                   (magnatune--sqlite-select (format select where)))))
    (-map (lambda (album)
            (let* ((alid (plist-get album :album_id))
                   (glist (magnatune--sqlite-select
                           (format (concat "select g.name from genres g, genres_albums ga "
                                           "where ga.album_id = %d and ga.genre_id = g.genre_id "
                                           "order by g.name collate nocase")
                                   alid)))
                   (genres (-filter 'stringp
                                    (-mapcat 'identity glist))))
              (plist-put album :genres genres)))
          albums)))

(defun magnatune-list-albums (artist-or-id)
  "List albums of the artist given by ARTIST-OR-ID.
This may be a string, which is used to search exactly for this
artis. It may be a number, which is used as the artist id to look
one up. It may be a plist with property :artists_id with the
artists id to use."
  (cond
   ((listp artist-or-id)
    (let ((id (plist-get artist-or-id :artists_id)))
      (when id
        (magnatune--album-query (format "and ar.artists_id = %d" id)))))
   ((numberp artist-or-id)
    (magnatune--album-query (format "and ar.artists_id = %d" artist-or-id)))
   ((stringp artist-or-id)
    (let ((id (plist-get (car (magnatune-search-artist artist-or-id t))
                         :artists_id)))
      (when id
        (magnatune--album-query (format "and ar.artists_id = %d" id)))))))


(defun magnatune-search-albums (&optional query-or-id strict offset limit order)
  "Search albums using QUERY-OR-ID.
If STRICT is t, QUERY-OR-ID must match exactly the name of the
album. Otherwise search albums starting with QUERY-OR-ID. If
QUERY-OR-ID is a number, it is used to lookup the album by this
id. QUERY-OR-ID can also be a genre plist, then return all albums
tagged with this genre. The result is a list of plists."
  (cond
   ((magnatune--null-or-blank-p query-or-id)
    (magnatune--album-query nil offset limit order))
   ((numberp query-or-id)
    (magnatune--album-query
     (format "and al.album_id = %d" query-or-id) offset limit order))
   ((eq (magnatune-item-type query-or-id) 'genre)
    (magnatune--album-query (format " and ga.genre_id = %d"
                                     (plist-get query-or-id :genre_id))
                             offset limit order))
   ((and (stringp query-or-id) strict)
    (magnatune--album-query (format "and al.name = '%s'" query-or-id)
                             offset limit order))
   ((and (stringp query-or-id) (not strict))
    (magnatune--album-query (format "and al.name like '%s'" query-or-id)
                             offset limit order))
   (t (user-error "Unrecognised query: %s" (prin1 query-or-id)))))

(defun magnatune-get-album-info (id)
  (let ((select (format "select description from albums where album_id = %d" id)))
    (magnatune-with-sqlite-buffer select
      (goto-char (point-min))
      (search-forward "description = ")
      (delete-region (point-min) (point))
      (s-trim (buffer-string)))))


(defun magnatune-list-songs (album)
  "List all songs of the given ALBUM.
ALBUM may be a plist with an :album_id property, the album id or
a string denoting the album name."
  (let ((select (concat "select s.song_id,s.album_id,s.name,s.track_no,s.duration,s.mp3, "
                        " al.name as album, ar.name as artist, ar.homepage as artist_page, "
                        " ar.artists_id, al.sku, al.upc, al.release_date, al.popularity "
                        "from songs s, albums al, artists ar "
                        "where al.artist_id = ar.artists_id "
                        "  and s.album_id = al.album_id and s.album_id = %d "
                        "order by s.track_no"))
        (where (cond
                ((listp album) (plist-get album :album_id))
                ((numberp album) album)
                (t (plist-get (car (magnatune-search-albums album t)) :album_id)))))
    (when where
      (magnatune--sqlite-select (format select where)))))

(defun magnatune-list-genres ()
  "Lists all available genres."
  (let ((select (concat "select g.genre_id, g.name, count(ga.album_id) as albums "
                        "from genres g, genres_albums ga "
                        "where g.genre_id = ga.genre_id "
                        "group by (g.genre_id) "
                        "order by g.name")))
    (magnatune--sqlite-select select)))


(defun magnatune-make-stream-url (song &optional free-p)
  "Return the stream url for a given SONG.
Return the membership url (if applicable) unless FREE-P is
truthy. SONG may be a list of songs or a single song, which is
either a plist with an :mp3 property or a string. Return a single
url or a list of urls depending on the input."
  (let* ((names (-map (lambda (s) (cond
                              ((eq (magnatune-item-type s) 'song)
                               (plist-get s :mp3))
                              ((stringp s) s)
                              (t (error "SONG is not a string or song or list thereof."))))
                      (if (eq (magnatune-item-type song) 'song)
                          (list song) (-list song))))
         (bases (-map 'file-name-sans-extension names))
         (urls nil))
    (if (and magnatune-username magnatune-password (not free-p))
        (setq urls
              (-map (lambda (base)
                      (magnatune--make-url
                       magnatune-member-url-fmt
                       magnatune-username
                       magnatune-password
                       base
                       magnatune-streaming-format))
                    bases))
      (setq urls
            (-map (lambda (base)
                    (magnatune--make-url
                     magnatune-free-url-fmt
                     base
                     magnatune-streaming-format))
                  bases)))
    (if (null (cdr urls))
        (car urls)
      urls)))

(defun magnatune-album-stream-urls (album-or-id &optional free-p)
  "Return the stream urls for all songs in the given ALBUM-OR-ID.
If FREE-P is truthy always return the free urls, even with
configured membership."
  (let* ((albums (magnatune-search-albums album-or-id))
         (songs  (-mapcat 'magnatune-list-songs albums)))
    (-list (magnatune-make-stream-url songs free-p))))

(defun magnatune-artist-stream-urls (artist-or-id &optional free-p)
  "Return all stream urls for all songs of ARTIST-OR-ID.
If FREE-P is truthy always return the free urls, even with
configured membership."
  (let* ((artists (magnatune-search-artist artist-or-id))
         (albums (-mapcat 'magnatune-list-albums artists))
         (songs (-mapcat 'magnatune-list-songs albums)))
    (-list (magnatune-make-stream-url songs free-p))))


;; simple ui ------------------------------------------------------

(defvar magnatune-browse-mode-hook nil)

(defvar magnatune-browse-mode-map nil)

(defvar magnatune--line-start-regexp "(id [0-9]+ chunk [0-9]+ index [0-9]+)")

(defun magnatune-browse-mode ()
  "Mode for special buffer for browsing magnatune's catalog.
\\{magnatune-browse-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'magnatune-browse-mode)
  (setq mode-name "Magnatune")
  (setq fill-column magnatune-column-width)
  (set (make-local-variable 'magnatune--update-fn) nil)
  (set (make-local-variable 'magnatune--query) nil)
  (set (make-local-variable 'magnatune--offset) nil)
  (set (make-local-variable 'magnatune--limit) nil)
  (set (make-local-variable 'magnatune--order) nil)
  (set (make-local-variable 'magnatune--results) nil)
  (set (make-local-variable 'magnatune--truncated-items) nil)
  (setq buffer-read-only t)
  (use-local-map magnatune-browse-mode-map)
  (run-hooks 'magnatune-browse-mode-hook))


(defun magnatune--string (obj)
  "Make OBJ a string."
  (cond
   ((stringp obj) obj)
   ((numberp obj) (number-to-string obj))
   (t (prin1-to-string obj))))

(defun magnatune--format-time (seconds)
  (if (numberp seconds)
      (format "%02d:%02d" (/ seconds 60) (% seconds 60))
    "--:--"))


(defun magnatune--insert-image-handler (_status buf position url)
  (let ((handl (mm-dissect-buffer (current-buffer) t)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char position)
          (when (string-prefix-p url (thing-at-point 'line t))
            (delete-char (length url))
            (condition-case nil
                (insert-image (mm-get-image handl))
              (error
               (message "Error loading image %s" url)
               (kill-new url)
               nil))))))))

(defun magnatune--insert-image (url &optional buffer pos)
  "Insert image at URL into BUFFER at POS. It first inserts URL
in BUFFER at POS and then retrieves the image contents
async. Once finished it checks whether the url is still the
same (otherwise it has been changed in the meantime) and if so
replaces the url with its contents."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (or pos (point-min)))
    (insert imgurl "\n"))
  (url-retrieve url
		'magnatune--insert-image-handler
		(list (or buffer (current-buffer))
		      (or pos (point-min))
                      url)))

(defun magnatune-insert-artist (artist chunk index)
  (let ((id (format "(id %d chunk %d index %d)"
                    (plist-get artist :artists_id) chunk index))
        (name (magnatune--string (plist-get artist :name)))
        (descr (plist-get artist :description))
        (albums (plist-get artist :albums))
        (songs (plist-get artist :songs))
        (len (plist-get artist :length)))
    (put-text-property 0 (length name)
                       'font-lock-face font-lock-keyword-face name)
    (put-text-property 0 (length id) 'invisible t id)
    (insert id)
    (insert name "\n")
    (let ((beg (point)))
      (put-text-property 0 (length descr)
                         'font-lock-face 'font-lock-doc-string-face descr)
      (insert descr "\n")
      (fill-region beg (point)))
    (let ((info (format "%d albums, %d songs, %smin\n"
                        albums songs (magnatune--format-time len))))
      (put-text-property 0 (length info)
                         'font-lock-face font-lock-comment-face info)
      (insert info))
    (insert-char ?― magnatune-column-width) ;;HORIZONTAL SCAN LINE-3
    (insert-char ?\n)))

(defun magnatune-insert-album-head (results)
  (let* ((album (car results))
         (artist (magnatune--string (plist-get album :artist)))
         (artistid (plist-get album :artists_id))
         (homepage (magnatune--make-url magnatune-artist-page-url-fmt
                                         (plist-get album :artist_page)))
         (photo (plist-get album :artist_photo))
         (name (magnatune--string (plist-get album :name)))
         (imgurl (concat magnatune-band-photo-url-fmt photo)))
    (when album
      (magnatune--insert-image imgurl)
      (put-text-property 0 (length artist) 'font-lock-face 'bold artist)
      (insert artist "\n" homepage "\n\n")
      (let ((beg (point)))
        (insert (magnatune-get-artist-info artistid) "\n\n")
        (fill-region beg (point))))))

(defun magnatune-insert-album (album chunk index &optional with-artist-p)
  (let ((id (format "(id %d chunk %d index %d)"
                    (plist-get album :album_id) chunk index))
        (name (magnatune--string (plist-get album :name)))
        (nametrunc nil)
        (artist (magnatune--string (plist-get album :artist)))
        (release (plist-get album :release_date))
        (pop (plist-get album :popularity))
        (songs (plist-get album :songs))
        (genres (mapconcat 'identity (plist-get album :genres) ", "))
        (len (magnatune--format-time (plist-get album :length))))
    (when with-artist-p
      (setq name (concat name " | " artist)))
    (setq nametrunc (s-truncate (- magnatune-column-width (length genres) 3)
                                name))
    (unless (equal name nametrunc)
      (setq magnatune--truncated-items
            (cons (cons (plist-get album :album_id) name)
                  magnatune--truncated-items)))
    (put-text-property 0 (length nametrunc)
                       'font-lock-face font-lock-keyword-face nametrunc)
    (put-text-property 0 (length id) 'invisible t id)
    (let ((beg (point)))
      (insert id)
      (insert nametrunc)
      (while (< (- (point) beg)
                (+ (length id) (- magnatune-column-width (length genres))))
        (insert " "))
      (insert genres "\n"))
    (let ((str (format "%s, %d songs, %smin (Popularity: %d)\n"
                       (format-time-string "%h %d, %Y" (seconds-to-time release))
                       songs
                       len
                       pop)))
      (put-text-property 0 (length str) 'font-lock-face font-lock-comment-face str)
      (insert str))
    (insert-char ?― magnatune-column-width)
    (insert-char ?\n)))

(defun magnatune-insert-song-head (results)
  (let* ((song (car results))
         (album (magnatune--string (plist-get song :album)))
         (albumid (plist-get song :album_id))
         (artist (magnatune--string (plist-get song :artist)))
         (imgurl (magnatune--make-url magnatune-cover-url-fmt
                                     artist
                                     album
                                     magnatune-cover-size))
         (str (format "%s | %s" album artist)))
    (magnatune--insert-image imgurl)
    (put-text-property 0 (length str) 'font-lock-face 'bold str)
    (insert str "\n\n")
    (let ((beg (point)))
      (insert (magnatune-get-album-info albumid) "\n\n")
      (fill-region beg (point)))))

(defun magnatune-insert-song (song chunk index)
  (let* ((id (format "(id %d chunk %d index %d)" (plist-get song :song_id) chunk index))
         (name (magnatune--string (plist-get song :name)))
         (track (plist-get song :track_no))
         (len (plist-get song :duration))
         (nametrunc (s-truncate (- magnatune-column-width 5 7) name))
         (str (format "%3d  %s" track nametrunc))
         (time (magnatune--format-time len))
         (beg (point)))
    (put-text-property 0 (length id) 'invisible t id)
    (insert id)
    (insert str)
    (unless (eq name nametrunc)
      (setq magnatune--truncated-items
            (cons (cons (plist-get song :song_id) name)
                  magnatune--truncated-items)))
    (while (< (- (point) beg) (+ (- magnatune-column-width 5) (length id)))
      (insert " "))
    (insert time "\n")
    (insert-char ?― magnatune-column-width)
    (insert-char ?\n)))

(defun magnatune-insert-genre (genre chunk index)
  (let* ((id (format "(id %d chunk %d index %d)"
                     (plist-get genre :genre_id) chunk index))
         (name (magnatune--string (plist-get genre :name)))
         (albums (plist-get genre :albums))
         (beg (point)))
    (put-text-property 0 (length name)
                       'font-lock-face font-lock-keyword-face name)
    (put-text-property 0 (length id) 'invisible t id)
    (insert id)
    (insert name "\n")
    (let ((info (format "%d albums\n" albums)))
      (put-text-property 0 (length info)
                         'font-lock-face font-lock-comment-face info)
      (insert info))
    (insert-char ?― magnatune-column-width)
    (insert-char ?\n)))

(defun magnatune--display-truncated-lines ()
  (let* ((item (magnatune-get-item-at-point))
         (id (magnatune-item-id item))
         (cell (assoc id magnatune--truncated-items)))
    (when cell
      (message "%s" (cdr cell)))))

(add-hook 'magnatune-browse-move-hook
          'magnatune--display-truncated-lines)


(defun magnatune--insert-elements (results chunk insert-fn &optional head-fn foot-fn)
  (let ((inhibit-read-only t))
    (when (functionp head-fn)
      (funcall head-fn results))
    (let ((index 0))
      (-each results
        (lambda (item)
          (funcall insert-fn item chunk index)
          (setq index (1+ index)))))
    (when (functionp foot-fn)
      (funcall foot-fn results))))


(defun magnatune--make-all-artists-buffer ()
  "Get or create the buffer displaying a list of artists."
  (let ((buff (get-buffer-create "*magnatune:all-artists*")))
    (with-current-buffer buff
      (unless (eq major-mode 'magnatune-browse-mode)
        (magnatune-browse-mode)
        (setq magnatune--offset 0)
        (setq magnatune--limit 200)
        (setq magnatune--update-fn
              (lambda ()
                (let ((result (magnatune-search-artist magnatune--query
                                                       nil
                                                       magnatune--offset
                                                       magnatune--limit)))
                  (setq magnatune--results
                        (reverse (cons result magnatune--results)))
                  (magnatune--insert-elements result
                                               (1- (length magnatune--results))
                                               'magnatune-insert-artist))
                (let* ((keys (where-is-internal 'magnatune-next-results))
                       (str (s-join ", " (-map 'key-description keys))))
                  (message "If you miss some artists, press %s to fetch more."
                           str))))))
    buff))

(defun magnatune--make-artist-buffer ()
  "Get or create the buffer displaying details about an artist."
  (let ((buff (get-buffer-create "*magnatune:artist*")))
    (with-current-buffer buff
      (unless (eq major-mode 'magnatune-browse-mode)
        (magnatune-browse-mode)
        (setq magnatune--update-fn
              (lambda ()
                (let ((result (magnatune-list-albums magnatune--query)))
                  (setq magnatune--results
                        (reverse (cons result magnatune--results)))
                  (magnatune--insert-elements result
                                               (1- (length magnatune--results))
                                               'magnatune-insert-album
                                               'magnatune-insert-album-head))))))
    buff))

(defun magnatune--make-album-buffer ()
  "Get or create the buffer displaying details about an album."
  (let ((buff (get-buffer-create "*magnatune:album*")))
    (with-current-buffer buff
      (unless (eq major-mode 'magnatune-browse-mode)
        (magnatune-browse-mode)
        (setq magnatune--update-fn
              (lambda ()
                (let ((result (magnatune-list-songs magnatune--query)))
                  (setq magnatune--results
                        (reverse (cons result magnatune--results)))
                  (magnatune--insert-elements result
                                               (1- (length magnatune--results))
                                               'magnatune-insert-song
                                               'magnatune-insert-song-head))))))
    buff))

(defun magnatune--make-all-albums-buffer ()
  "Get or create the buffer displaying a arbitrary list of albums."
  (let ((buff (get-buffer-create "*magnatune:all-albums*")))
    (with-current-buffer buff
      (unless (eq major-mode 'magnatune-browse-mode)
        (magnatune-browse-mode)
        (setq magnatune--offset 0)
        (setq magnatune--limit 200)
        (setq magnatune--order 'release_date)
        (setq magnatune--update-fn
              (lambda ()
                (let ((result (magnatune-search-albums magnatune--query
                                                       nil
                                                       magnatune--offset
                                                       magnatune--limit
                                                       magnatune--order)))
                  (setq magnatune--results
                        (reverse (cons result magnatune--results)))
                  (magnatune--insert-elements
                   result
                   (1- (length magnatune--results))
                   (lambda (album chunk index)
                     (magnatune-insert-album album chunk index t))))
                (let* ((keys (where-is-internal 'magnatune-next-results))
                       (str (s-join ", " (-map 'key-description keys))))
                  (message "If you miss some albums, press %s to fetch more."
                           str))))))
    buff))

(defun magnatune--make-all-genres-buffer ()
  "Get or create the buffer that displays a list of genres."
  (let ((buff (get-buffer-create "*magnatune:all-genres*")))
    (with-current-buffer buff
      (unless (eq major-mode 'magnatune-browse-mode)
        (magnatune-browse-mode)
        (setq magnatune--update-fn
              (lambda ()
                (let ((result (magnatune-list-genres)))
                  (setq magnatune--results
                        (reverse (cons result magnatune--results)))
                  (magnatune--insert-elements
                   result
                   (1- (length magnatune--results))
                   'magnatune-insert-genre))))))
    buff))

(defun magnatune-browse-buffer-type (&optional buffer)
  "Return the type of a magnatune BUFFER.
This is either 'all-artists, 'all-genres, 'all-albums, 'artist or
'album."
  (let* ((buf (or buffer (current-buffer)))
         (name (buffer-name buf))
         (type (when (s-starts-with-p "*magnatune:" name)
                 (s-split ":" name t))))
    (when (and type (= 2 (length type)))
      (intern (replace-regexp-in-string "\\\*$" "" (cadr type))))))


(defun magnatune-browse-update-view (&optional arg)
  "Clears the buffer and reloads all data.
With prefix arg, clear the query and set offset to 0."
  (interactive "P")
  (when arg
    (setq magnatune--query nil)
    (when magnatune--offset
      (setq magnatune--offset 0)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not (and magnatune--offset magnatune--limit))
        (funcall magnatune--update-fn)
      (let ((lastoff magnatune--offset))
        (setq magnatune--offset 0)
        (while (not (eq magnatune--offset lastoff))
          (funcall magnatune--update-fn)
          (setq magnatune--offset
                (+ magnatune--offset magnatune--limit)))
        (funcall magnatune--update-fn)))
    (goto-char (point-min))))

(defun magnatune-next-results ()
  "Get the next chunk of results."
  (interactive)
  (when (and (eq major-mode 'magnatune-browse-mode)
             magnatune--limit
             magnatune--offset)
    (setq magnatune--offset (+ magnatune--offset magnatune--limit))
    (save-excursion
      (goto-char (point-max))
      (funcall magnatune--update-fn))))

;;;###autoload
(defun magnatune-browse (arg)
  "The entry point into magnatune's catalog.

Asks to either list artists, albums or genres. For artists and
albums a prefix arg asks for an initial query to narrow down the
list."
  (interactive "P")
  (let* ((what (read-char "(A)rtists, A(l)bums or (G)enres? "))
         (buffer (if (or (eq what ?a) (eq what ?A))
                     (magnatune--make-all-artists-buffer)
                   (if (or (eq what ?l) (eq what ?L))
                       (magnatune--make-all-albums-buffer)
                     (magnatune--make-all-genres-buffer))))
         (type (magnatune-browse-buffer-type buffer))
         (ask-query (and arg (not (equal type 'all-genres)))))
    (with-current-buffer buffer
      (let ((old-query magnatune--query))
        (when ask-query
          (setq magnatune--query (read-string "Query: ")))
        ;; only reload if query differs or the buffer has no results
        (when (or (not (eq old-query magnatune--query))
                  (eq nil magnatune--results))
          (when (and magnatune--offset magnatune--limit)
            (setq magnatune--offset 0)
            (setq magnatune--limit 200))
          (magnatune-browse-update-view))))
    (unless (eq (current-buffer) buffer)
      (pop-to-buffer buffer))))


(defun magnatune--buffer-window (buffer)
  "Find the window displaying BUFFER."
  (-find (lambda (win)
           (eq (window-buffer win) buffer))
         (window-list)))

(defun magnatune-find-next (&optional item pop)
  "Find the next items to the ITEM at point.
If on the artist search view, go to the albums of the current
artist. If on an album, go to its songs. If POP is truthy,
`pop-to-buffer'."
  (interactive (list (magnatune-get-item-at-point)))
  (let* ((type (magnatune-browse-buffer-type))
         (item (or item (magnatune-get-item-at-point)))
         (next (cond
                ((eq type 'all-artists)
                 (magnatune--make-artist-buffer))
                ((eq type 'artist)
                 (magnatune--make-album-buffer))
                ((eq type 'all-albums)
                 (magnatune--make-album-buffer))
                ((eq type 'all-genres)
                 (magnatune--make-all-albums-buffer))
                ((eq type 'album)
                 (current-buffer))
                ((not type)
                 (user-error "Not a magnatune buffer."))
                (t (error "Buffer %s not handled." type)))))
    (unless (eq next (current-buffer))
      (with-current-buffer next
        (unless (equal magnatune--query item)
          (setq magnatune--query item)
          (magnatune-browse-update-view))
        (if pop
            (pop-to-buffer next)
          (let ((win (magnatune--buffer-window next)))
            (if (and win (window-live-p win))
                (select-window win)
              (switch-to-buffer next))))))))

(defun magnatune-find-next-other-window (&optional item)
  "Find the next items to the ITEM at point in another window.
The focus moves back to the previous window."
  (interactive (list (magnatune-get-item-at-point)))
  (let ((win (selected-window)))
    (magnatune-find-next item t)
    (select-window win)))


(defun magnatune-browse-next (&optional n)
  "Go to the next N items in the list."
  (interactive "p")
  (let ((count (or n 1))
        (type (magnatune-browse-buffer-type))
        (pos nil))
    (while (> count 0)
      (goto-char (line-end-position))
      (if (search-forward-regexp magnatune--line-start-regexp nil t)
          (progn
            (forward-line 0)
            (setq pos (point)))
        (forward-line 0))
      (setq count (1- count)))
    (when pos
      (run-hooks 'magnatune-browse-move-hook))
    pos))

(defun magnatune-browse-prev (&optional n)
  "Go to the previous N items in the list."
  (interactive "p")
  (let ((count (or n 1))
        (pos nil))
    (while (> count 0)
      (unless (looking-at magnatune--line-start-regexp)
        (search-backward-regexp magnatune--line-start-regexp nil t))
      (let ((prev (search-backward-regexp magnatune--line-start-regexp nil t)))
        (if prev
            (progn
              (forward-line 0)
              (setq pos (point)))
          (goto-char (point-min))))
      (setq count (1- count)))
    (when pos
      (run-hooks 'magnatune-browse-move-hook))
    pos))

(defun magnatune-browse-jump (prefix)
  "Jump to the item in the list that starts with PREFIX."
  (interactive "sStarting with: ")
  (when (eq (magnatune-browse-buffer-type) 'all-artists)
    (unless (search-forward-regexp
             (concat magnatune--line-start-regexp prefix) nil t)
      (while (search-backward-regexp
              (concat magnatune--line-start-regexp prefix) nil t)))
    (forward-line 0)
    (run-hooks 'magnatune-browse-move-hook)
    (point)))

(defun magnatune-browse-jump-generate ()
  "Generate `magnatune-browse-jump' functions for each capital letter."
  (let ((forms (-map
                (lambda (el)
                  `(defun ,(intern (format "magnatune-browse-jump-%c" el)) ()
                     (interactive)
                     ,(format "Jump to the first occurence of %c." el)
                     (magnatune-browse-jump ,(format "%c" el))))
                (-iterate '1+ 65 26))))
   forms))

(-each (magnatune-browse-jump-generate) 'eval)

(defun magnatune-get-item-at-point ()
  "Return the current item."
  (interactive)
  (when (eq major-mode 'magnatune-browse-mode)
    (save-excursion
      (forward-line 0)
      (unless (looking-at magnatune--line-start-regexp)
        (search-backward-regexp magnatune--line-start-regexp nil t)
        (forward-line 0))
      (search-forward-regexp magnatune--line-start-regexp nil t)
      (let ((ids (read (match-string 0))))
        (nth (plist-get ids 'index)
             (nth (plist-get ids 'chunk) magnatune--results))))))


(defun magnatune-external-url-hook (urls &optional _arg)
  "Provided hook that hands all given URLS to `magnatune-external-player'."
  (let ((proc (get 'magnatune--external-url-hook :proc))
        (args (append (list (concat "magnatune-" magnatune-external-player)
                            nil
                            magnatune-external-player)
                      urls)))
    (when (and proc (process-live-p proc))
      (kill-process proc))
    (put 'magnatune--external-url-hook
         :proc
         (apply 'start-process args))
    (message "Playing %d songs with %s"
             (length urls)
             magnatune-external-player)))

(defun magnatune-mpc-url-hook (urls &optional arg)
  "Provided hook that appends all URLS to a local mpd. With
prefix arg, the playlist is cleared first."
  (require 'mpc)
  (when arg
      (mpc-proc-cmd "clear"))
  (mpc-cmd-add urls)
  (when arg
    (mpc-cmd-play))
  (message "Added %d songs to mpd" (length urls)))

(defun magnatune-emms-url-hook (urls &optional arg)
  "Provided hook that pushes all urls to EMMS current
playlist. With prefix arg, the playlist is cleared first."
  (unless (fboundp 'emms)
    (user-error "EMMS is not available."))
  (unless emms-playlist-buffer
    (emms-playlist-new))
  (with-current-emms-playlist
    (when arg
      (emms-playlist-clear))
    (-each urls (lambda (url)
                  (emms-playlist-insert-track (emms-track 'url url))))
    (when arg
      (when emms-player-playing-p
        (emms-stop))
      (emms-playlist-select-first)
      (emms-start))))

(defun magnatune--run-url-hooks (urls arg)
  (unless magnatune-open-urls-hook
    (message "No functions set in `magnatune-open-urls-hook'."))
  (run-hook-with-args 'magnatune-open-urls-hook urls arg))

(defun magnatune-urls-at-point (&optional free-p)
  "Return a list of all stream urls for the entry at point."
  (let* ((thing (magnatune-get-item-at-point))
         (type (magnatune-item-type thing)))
    (when thing
      (cond
       ((eq type 'artist)
        (magnatune-artist-stream-urls (plist-get thing :artists_id) free-p))
       ((eq type 'album)
        (magnatune-album-stream-urls (plist-get thing :album_id) free-p))
       ((eq type 'song)
        (-list (magnatune-make-stream-url thing free-p)))
       (t (error "Item not recognised."))))))

(defun magnatune-copy-urls-at-point (arg)
  "Copy all stream urls of item at point into kill ring."
  (interactive "P")
  (let ((urls (magnatune-urls-at-point arg)))
    (when urls
      (kill-new (mapconcat 'identity urls "\n"))
      (message "Copied %d urls to kill ring." (length urls)))))

(defun magnatune-open-item-at-point (arg)
  "Open the item, which may be a song, album or artist at point.
The stream urls of the item are collected and the hook
`magnatune-url-handler-hooks' is called on them."
  (interactive "P")
  (let ((urls (magnatune-urls-at-point)))
    (when urls
      (magnatune--run-url-hooks urls arg))))

(defun magnatune-find-artist-of-item (pop-p)
  "Visit the buffer showing details about the artist of the current item.
Pop to this buffer if POP-P is truthy.
This is useful when looking at the songs of an album when not
coming from the artist list."
  (interactive "P")
  (let ((bufftype (magnatune-browse-buffer-type)))
    (cond
     ((eq bufftype 'all-artists)
      (magnatune-find-next))
     ((eq bufftype 'artist) nil)
     (t
      (let* ((item (magnatune-get-item-at-point))
             (id  (or (plist-get item :artist_id)
                      (plist-get item :artists_id))))
        (when id
          (with-current-buffer (magnatune--make-artist-buffer)
            (setq magnatune--query id)
            (magnatune-browse-update-view)
            (if pop-p
                (pop-to-buffer (current-buffer))
              (switch-to-buffer (current-buffer))))))))))

(defun magnatune-download-album (sku &optional format targetdir)
  "Download the zip file of the album given with SKU.
Download files with FORMAT, but use `magnatune-download-format'
if not specified. Put the file in TARGET_DIR or
`magnatune-download-folder'. Return the file name."
  (interactive (list
                (let* ((item (magnatune-get-item-at-point))
                       (sku (plist-get item :sku)))
                  (or sku (user-error "Not on an album.")))))
  (when (not (and magnatune-password magnatune-username))
    (user-error "Downloading albums requires a magnatune membership."))
  (when (yes-or-no-p (format "Download the album %s to %s? "
                             sku (or targetdir magnatune-download-folder)))
    (let* ((fmt (or format magnatune-download-format))
           (url (magnatune--make-url magnatune-download-url-fmt
                                     sku
                                     fmt)))
      (message "Downloading …")
      (let ((url-request-extra-headers
             `(("Authorization" . ,(concat "Basic "
                                           (base64-encode-string
                                            (concat magnatune-username ":"
                                                    magnatune-password))))))
            (file (expand-file-name (concat sku ".zip")
                                    (or targetdir
                                        magnatune-download-folder))))
        (url-copy-file url file)
        (run-hook-with-args 'magnatune-download-hook file)
        (message "Downloaded %s." sku)
        file))))

(defun magnatune-sort-all-albums ()
  "Sort the albums list.
This is only applicable for the 'all-albums' buffer."
  (interactive)
  (when (eq (magnatune-browse-buffer-type) 'all-albums)
    (let ((new (read-char "Sort by n) name, r) release date, p) popularity?"))
          (old magnatune--order))
      (cond
       ((-elem-index new '(?n ?N))
        (setq magnatune--order 'name))
       ((-elem-index new '(?r ?R))
        (setq magnatune--order 'release_date))
       ((-elem-index new '(?p ?P))
        (setq magnatune--order 'popularity))
       (t (user-error "Uknown choice.")))
      (unless (eq old magnatune--order)
        (magnatune-browse-update-view)))))

(defun magnatune-get-artist-page-url (item)
  "Return the home page url of the current artist."
  (let* ((type (magnatune-item-type item))
         (part (if (eq type 'artist)
                   (plist-get item :homepage)
                 (plist-get item :artist_page))))
    (if part
        (magnatune--make-url magnatune-artist-page-url-fmt
                             part)
      (user-error "Cannot get a url for a %s" type))))

(defun magnatune-get-album-page-url (item)
  "Return the web page url of the current album."
  (let* ((part (plist-get item :sku)))
    (if part
        (magnatune--make-url magnatune-album-page-url-fmt
                             part)
      (user-error "Cannot get an album url from a %s" (magnatune-item-type item)))))

(defun magnatune-browse-artist-or-album-page (char)
  (interactive (list (read-char
                      "Browse (a)rtist page or a(l)bum page. Capital letter copies the url.")))
  (let ((url (if (or (eq char ?a) (eq char ?A))
                 (magnatune-get-artist-page-url (magnatune-get-item-at-point))
               (magnatune-get-album-page-url (magnatune-get-item-at-point)))))
    (when url
      (if (>= char 97)
          (browse-url url)
        (kill-new url)
        (message "Copied %s." url)))))

(defun magnatune-list-magnatune-buffers ()
  "List all live magnatune buffers."
  (-filter 'magnatune-browse-buffer-type
           (buffer-list)))

(defun magnatune-list-magnatune-windows ()
  "Return a list of windows with current magnatune buffer."
  (-filter (lambda (win)
             (magnatune-browse-buffer-type (window-buffer win)))
           (window-list)))

(defun magnatune-kill-all-buffers ()
  "Kill all magnatune buffers."
  (interactive)
  (let ((buffers (magnatune-list-magnatune-buffers)))
    (-each buffers
      (lambda (buf) (kill-buffer buf)))))

(defun magnatune-quit-all-windows (kill)
  "Quit all magnatune windows and bury its buffers.

If KILL is non nil, all magnatune buffers are killed. Otherwise
`quit-window' is called on all windows displaying a magnatune
buffer. This is repeated until there is no such live window
left."
  (interactive "P")
  (if kill
      (magnatune-kill-all-buffers)
    (let ((wins (magnatune-list-magnatune-windows)))
      (-each wins
        (lambda (win) (quit-window nil win)))
      (when wins
        (magnatune-quit-all-windows kill)))))


;; follow mode
(defun magnatune--follow-item ()
  (when (and magnatune-browse-follow-mode
             (not (eq (magnatune-browse-buffer-type) 'album)))
    (magnatune-find-next-other-window (magnatune-get-item-at-point))))

(add-hook 'magnatune-browse-move-hook 'magnatune--follow-item)

(define-minor-mode magnatune-browse-follow-mode
  "A minor mode to immediatly follow the current item in another window.

Toggle Magnatune-Browse-Follow mode on or off.
With a prefix argument ARG, enable Magnatune-Browse-Follow mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil)

(define-global-minor-mode magnatune-global-browse-follow-mode
  magnatune-browse-follow-mode
  (lambda () (magnatune-browse-follow-mode 1)))

(unless magnatune-browse-mode-map
  (setq magnatune-browse-mode-map (make-sparse-keymap))
  (suppress-keymap magnatune-browse-mode-map)
  (define-key magnatune-browse-mode-map (kbd "q") 'quit-window)
  (define-key magnatune-browse-mode-map (kbd "C-q") 'magnatune-quit-all-windows)
  (define-key magnatune-browse-mode-map (kbd "g") 'magnatune-browse-update-view)
  (define-key magnatune-browse-mode-map (kbd "e") 'magnatune-next-results)
  (define-key magnatune-browse-mode-map (kbd "s") 'magnatune-sort-all-albums)
  (define-key magnatune-browse-mode-map (kbd "C-f") 'magnatune-global-browse-follow-mode)
  (define-key magnatune-browse-mode-map (kbd "n") 'magnatune-browse-next)
  (define-key magnatune-browse-mode-map (kbd "p") 'magnatune-browse-prev)
  (define-key magnatune-browse-mode-map (kbd "RET") 'magnatune-find-next)
  (define-key magnatune-browse-mode-map (kbd "C-<return>") 'magnatune-find-next-other-window)
  (define-key magnatune-browse-mode-map (kbd "\C-o") 'magnatune-open-item-at-point)
  (define-key magnatune-browse-mode-map (kbd "\C-a") 'magnatune-find-artist-of-item)
  (define-key magnatune-browse-mode-map (kbd "\C-b") 'magnatune-browse-artist-or-album-page)
  (define-key magnatune-browse-mode-map (kbd "SPC") 'scroll-up-line)
  (define-key magnatune-browse-mode-map (kbd "\C-c") 'magnatune-copy-urls-at-point)
  (define-key magnatune-browse-mode-map (kbd "\C-d") 'magnatune-download-album)
  (define-key magnatune-browse-mode-map (kbd "/") 'magnatune-browse-jump)
  (-each (-iterate '1+ 65 26)
    (lambda (c)
      (define-key magnatune-browse-mode-map
        (kbd (format "%c" c))
        (intern (format "magnatune-browse-jump-%c" c))))))

(provide 'magnatune)

;;; magnatune.el ends here
