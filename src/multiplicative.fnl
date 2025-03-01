;;;; Multiplicative - Multiply the power of mpv.

(local fennel (require :fennel))
(require "mp")
(local utils (require "mp.utils"))
(local options (require "mp.options"))
(local msg (require "mp.msg"))
(local input (require "mp.input"))
(local assdraw (require "mp.assdraw"))

(var opts {}) ; temporarily empty until reset later in the script

;;; Utility

(fn dprint [obj]
  "Print an object and return it."
  (print obj)
  obj)

;; from https://github.com/bakpakin/Fennel/wiki/Block-return
(macro return-from [name ...]
  `(error {:target ,name
           :data (#(doto [$...] (tset :n (select "#" $...))) ,...)}))

(macro block [name ...]
  `(let [,name (setmetatable {} {:__name "block"
                                 :__fennelview #(.. "#<" (_G.tostring $) ">")})
         pack# #(doto [$...] (tset :n (select "#" $...)))
         unpack# (or _G.table.unpack _G.unpack)
         (ok# res#) ,(if (. (get-scope) :vararg)
                         `(pcall (fn [...] (pack# (do ,...))) ...)
                         `(pcall (fn [] (pack# (do ,...)))))]
     (if ok#
         (unpack# res# 1 res#.n)
         (match res#
           {:target ,name
            :data data#}
           (unpack# data# 1 data#.n)
           _# (error _#)))))

;;; Tables

(fn contains? [table item]
  "True if TABLE (a sequential table) contains ITEM."
  (let [len (length table)]
    (var found false)
    (var idx 0)
    (while (and (not found)
                (< idx len))
      (set idx (+ idx 1))
      (when (= item (. table idx))
        (set found true)))
    found))

(fn table-append [& tables]
  "Get a table whose elements consist of the elements of each table in TABLES."
  (let [res []]
    (each [_ tab (ipairs tables)]
      (each [_ v (ipairs tab)]
        (table.insert res v)))
    res))

(fn table-kvappend [& tables]
  "Get a table whose elements consist of the elements of each table in TABLES."
  (let [res []]
    (each [_ tab (ipairs tables)]
      (each [k v (pairs tab)]
        (set (. res k) v)))
    res))

;;; Strings

(fn string-starts-with? [string prefix]
  "True if STRING starts with PREFIX."
  (= prefix (string.sub string 1 (length prefix))))

(fn split [input separator count]
  "Split INPUT (a string) by SEPARATOR (a Lua pattern), optionally up to a maximum of COUNT times. Note that empty strings will be removed from the result."
  (let [separator (or separator "%s")
        result []]
    (var rest input)
    (var (start end) (values nil nil))
    (while (and (or (= nil count) (< (length result) (- count 1)))
                (do (var (start* end*) (string.find rest separator))
                    (set start start*)
                    (set end end*)
                    (and start* end*)))
      (when (not (= end 1))
        (table.insert result (string.sub rest 1 (- start 1))))
      (set rest (string.sub rest (+ 1 end))))
    (while (= (string.find rest separator) 1)
      (set rest (string.sub rest 2)))
    (table.insert result rest)
    result))

(fn split-pattern [input separator]
  "Split INPUT (a string) by SEPARATOR (a Lua pattern)."
  (let [separator (or separator "%s")
        result []]
    (each [str _ (string.gmatch input (.. "([^" separator "]+)"))]
      (table.insert result str))
    result))

;;; MPV

(fn msg-trace [message]
  "Log a trace message."
  (msg.log "trace" message))

(fn msg-debug [message]
  "Log a debug message."
  (msg.log "debug" message))

(fn msg-verbose [message]
  "Log a verbose message."
  (msg.log "v" message))

(fn msg-info [message]
  "Log an informational message."
  (msg.log "info" message))

(fn msg-warn [message]
  "Log a warning message."
  (msg.log "warn" message))

(fn msg-error [message]
  "Log an error message."
  (msg.log "error" message))

(fn msg-fatal [message]
  "Log a fatal message."
  (msg.log "fatal" message))

(fn osd [message]
  "Show a message on the OSD."
  (mp.osd_message message))

(fn message [message]
  "Log an informational message and show it on the OSD."
  (msg-info message)
  (osd message))

(fn command-line-arguments []
  "Get the command line arguments of the current mpv process."
  (let [pid (utils.getpid)
        proc-info (system-processes-find-pid pid)]
    (. proc-info 4)))

;;; System
;; (needed here for `opts' default values)

(fn os-type []
  "Get a string representing the operating system type. Common values are windows, darwin (macos or ios), linux, android, and freebsd."
  (mp.get_property_native "platform"))

(fn unix-like? []
  "True if we're running on a Unix-like operating system."
  (contains? ["linux" "freebsd" "android" "darwin"] (os-type)))

(fn linux? []
  "True if we're running on a non-Android Linux-based operating system."
  (= "linux" (os-type)))

(fn windows? []
  "True if we're running on Windows."
  (= "windows" (os-type)))

(fn gui-type []
  "Get a string representing the GUI system type. Common values on Linux are x11 and wayland, while other platforms typically have the same value as the `os-type'."
  (if (linux?)
      (os.getenv "XDG_SESSION_TYPE")
      (os-type)))

(var subprocess utils.subprocess) ; mpv alias

(fn copy-to-clipboard [str]
  "Copy STR to the system clipboard."
  (subprocess {:args (. opts :copy-command) :stdin_data str :capture_stdout true :capture_stderr true}))

(fn system-processes []
  "Get a table of processes running on the system. Each element of the table is itself a table consisting of the process's PID, username, command, and command line.

See also: `system-processes-matching'"
  (let [subprocess-result (subprocess {:args ["ps" "--no-headers" "--columns" "1000" "-eo" "pid,user,comm,args"]
                                       :playback_only false
                                       :capture_stdout true
                                       :detach false})
        stdout (. subprocess-result :stdout)
        lines (split stdout "\n")]
    (icollect [_ line (ipairs lines)]
      (when (not (= "" line))
        (split line " " 4)))))

(fn system-processes-matching [process-name]
  "Get a table of processes running on the system whose process name matches PROCESS-NAME. Each element of the table is itself a table consisting of the process's PID, username, command, and command line.

See also: `system-processes'"
  (icollect [_ proc (ipairs (system-processes))]
    (when (string.match (. proc 3) process-name)
      proc)))

(fn system-processes-find-pid [pid]
  "Get a table of information about the system process whose process ID is PID, or nil if none found. The resulting table contains the process's PID, username, command, and command line."
  (let [res (icollect [_ proc (ipairs (system-processes))]
              (when (= (. proc 1) (utils.to_string pid))
                proc))]
    (when res
      (. res 1))))

(fn dbus-service-available? [service]
  "Get information about a dbus service, or nil if it is not available."
  (subprocess {:args ["busctl" "--user" "status" service] :capture_stdout true :capture_stderr true}))

;;; Paths and URLs

(fn unix-path? [string]
  "True if STRING looks like a Unix-style local file path."
  (= "/" (string.sub string 1 1)))

(fn windows-path? [string]
  "True if STRING looks like a Windows-style local file path."
  (string.match string "^[A-Z]:\\"))

(fn path-local? [string]
  "True if STRING looks like a local file path."
  (or (and (unix-like?)
           (unix-path? string))
      (and (windows?)
           (windows-path? string))))

(fn path-remote? [string]
  "True if STRING looks like a remote file path (i.e. an HTTP URL)."
  (or (= "http:/" (string.sub string 1 6))
      (= "https:/" (string.sub string 1 7))))

(fn path-absolute? [path]
  "True if PATH is a full path (i.e. starts with /, http:/, or https:/)."
  (or (= "/" (string.sub path 1 1))
      (= "http:/" (string.sub path 1 6))
      (= "https:/" (string.sub path 1 7))))

(fn path-expand-tilde [path]
  "Expand a tilde at the start of PATH into the home directory of the current or specified user."
  (if (and (= 1 (length path)) (= "~" path)) (os.getenv "HOME")
      (= "~/" (string.sub path 1 2)) (.. (os.getenv "HOME") (string.sub path 2))
      ;; FIX: handle other users' home directories? i.e. ~bob = /home/bob
      path))

(fn path-expand-remove-selves [path]
  "Remove unneeded \"/./\" from PATH."
  (let [(result replacements) (string.gsub path "/./" "/")]
    result))

(fn path-absolute [path]
  "Get the full path to PATH. If PATH is not a full path, we assume it's relative to the current working directory."
  (let [path (path-expand-tilde path)]
    (path-expand-remove-selves (if (path-absolute? path)
                                   path
                                   (.. (mp.get_property "working-directory") "/" path)))))

(fn path-directory [path] ; mpv wrapper
  "Get a table containing the directory and filename components of PATH."
  (let [(directory filename) (utils.split_path path)]
    directory))

(fn path-filename [path] ; mpv wrapper
  "Get the filename component of PATH (i.e. remove the directory component)."
  (let [(dir filename) (utils.split_path path)]
    filename))

(fn path-extension [path]
  "Get the file extension of PATH (not including the period)."
  (let [split-up (split path "[.]")]
    (. split-up (length split-up))))

(fn path-no-extension [path]
  "Remove the file extension (and period) from PATH."
  (let [split-up (split path "[.]")]
    (table.remove split-up (length split-up))
    (table.concat split-up ".")))

(fn path-join [path-1 path-2] ; mpv wrapper
  "Get the concatenation of PATH-1 and PATH-2. If PATH-2 is an absolute path, PATH-2 is returned as-is."
  (utils.join_path path-1 path-2))

(fn path-abbreviate [path]
  "Abbreviate path components where possible. For example, the user's home directory is replaced with ~."
  path) ; FIX

(fn url-protocol [url]
  "Get the protocol part of URL. Returns nil if no protocol is found; thus this can also be used to check if a string looks like a URL."
  (string.match url "^[a-z]-:/*")) ; FIX: trim the :/ off

(fn url-protocol-http? [url]
  "True if URL is http:// or https://."
  (contains? ["http://" "https://"] (url-protocol url)))

(fn url-domain [url]
  "Get the domain part of URL."
  (string.match url "^.-:/*([^/]+)"))

(fn url-path [url]
  "Get the path part of URL."
  (string.match url "^.-:/*[^/]+(/?[^%?]*)"))

(fn url-query [url]
  "Get the query part of URL."
  (string.match url "^.-:/*[^/]+/?[^%?]*(%?.*)")) ; FIX: doesn't work yet.

(fn file-exists? [filename]
  "Get a table of information about the file denoted by FILENAME, or nil if FILENAME does not exist."
  (let [(tab err) (utils.file_info filename)]
    tab))

(fn media-type [filename]
  "Get a generalized file type (i.e. audio, video, image, etc) of FILENAME, or the current media if no filename is specified."
  (if filename
      (case (string.lower (path-extension filename))
        ;; FIX: use image-exts, audio-exts, video-exts for this instead of hardcoding them:
        (where (or "bmp" "gif" "jpeg" "jpg" "png" "webp")) "image"
        (where (or "aac" "aif" "aiff" "flac" "m4a" "mp3" "ogg" "opus" "wav" "wma")) "audio"
        (where (or "avi" "flv" "m2t" "m4v" "mkv" "mov" "mp4" "ogv" "webm" "wmv")) "video")
      (if (mp.get_property_native (table.concat ["track-list/" (playlist-current-index) "/image"])) "image"
          (and (has-audio?) (not (has-video?))) "audio"
          (and (has-audio?) (has-video?)) "video")))

;;; Files

(fn delete-command []
  "Get the correct command for \"deleting\" a file. When the delete-by-trashing option is true, this will be the value of the trash-command option, otherwise it will be the value of the delete-command option."
  (. opts (if (. opts :delete-by-trashing)
              :trash-command
              :delete-command)))

(fn file-delete [file]
  "Delete FILE from the filesystem using the command specified by the delete-command option."
  (let [command (delete-command)
        path (path-absolute file)]
    (subprocess {:args [command path] :detach true})
    (message (.. command " " file))))

;;; Options

(set opts {;; The amount by which the pan keys should pan.
           :pan-amount 0.05
           ;; Whether to skip to the beginning or end of the playlist if playlist_next_dir can't find a different directory in the specified direction.
           :playlist-directory-skip-end true
           ;; Command to copy text to the clipboard via stdin.
           :copy-command (case (gui-type) ; FIX: when loading copy-command from the .conf file, split the string on spaces
                           "x11" ["xsel" "--input" "--clipboard"] ; ["xclip" "-i" "-selection" "clipboard"] ; not sure why xclip doesn't work
                           "wayland" ["wl-copy"]
                           "macos" ["pbcopy"])
           ;; The command used for deleting files.
           :delete-command "rm"
           ;; The command used for trashing files.
           :trash-command "trash-put"
           ;; Whether the "delete" command should trash instead of deleting.
           :delete-by-trashing true
           ;; Filename format to save screenshots as.
           :screenshot-filename-format "~/shot-${filename/no-ext}-${time-pos}-%NUM%.jpg" ; FIX: mp.utils.get_user_path might be useful for stuff like this?
           ;; Whether to record history to the history file.
           :record-history true
           ;; The filename to write history to.
           :history-log-filename (.. (os.getenv "XDG_DATA_HOME") "/mpv/log/mpv-history.log") ; FIX: what if XDG_DATA_HOME is not set? what if this dir doesn't exist?
           ;; Lua pattern of URL/paths to exclude from history.
           :history-exclusion-pattern "^$"
           ;; The command used for the `show-in-file-manager' command.
           :file-manager "xdg-open"
           ;; The command used for the `open-in-browser' command.
           :browser "xdg-open" ; FIX: turn this into a function which defaults to the correct "open" for each OS.
           ;; Additional arguments to pass to mpv processes spawned by the `open-directory' command.
           :open-directory-arguments "--profile=image-mode" ; FIX: use this?
           ;; Size of the title text.
           :title-text-size 48
           ;; Whether the status bar should be shown by default.
           :status-bar-enabled false
           ;; Automatically enable the status bar if this Fennel code evaluates to true when mpv starts.
           :status-bar-enable-when "false"
           ;; Margin for the status bar, in pixels.
           :status-bar-margin 2
           ;; Size of the status bar text.
           :status-bar-text-size 22
           ;; Whether to inhibit the screensaver while media with audio is playing.
           :screensaver-inhibit false ; default to false since mpv provides its own which users might already have enabled.
           ;; The number of seconds to pause before auto-exiting at end of playlist, or false to disable this behavior.
           :pre-exit-pause-time 5})

(options.read_options opts (mp.get_script_name) true)

;; meta

(fn fn-name [func]
  "Get the name of FUNC."
  (fennel.metadata:get func :fn-name))

(fn fn-arglist [func]
  "Get the arglist of FUNC."
  (fennel.metadata:get func :fnl/arglist))

(fn fn-doc [func]
  "Get the docstring for FUNC."
  (fennel.metadata:get func :fnl/docstring))

(macro cmd [name args & body]
  "Define a command.

At the moment, the only difference between this and a regular function is that the function's name is stored in its metadata."
  (let [name-string (tostring name)
        first (. body 1)
        has-docstring (= "string" (type first))
        doc (if has-docstring first nil)
        body (if has-docstring [(table.unpack body 2)] body)]
    `(do (global ,name (fn ,name ,args
                         {:fnl/docstring ,doc
                          :fnl/arglist ,args
                          :fn-name ,name-string}
                         ,(table.unpack body)))
         (set (. _G ,name-string) ,name))))

(fn key-bind [key command flags] ; FIX: should this be defined in terms of key-map-bind?
  "Bind KEY to COMMAND in the default keymap."
  ;; FIX: implement this?
  ;; (let [command (if (= "key" (. (fn-arglist command) 1)) ; FIX: need to figure out how to ensure the key is sent to commands whose first argument is "key".
  ;;                   (partial command key) ; FIX: i think this won't allow the function's docstring to be looked up by key.
  ;;                   command)]
  ;;   (tset keymap :bindings key command))
  (mp.add_key_binding key (fn-name command) command flags))

;;; Events and Observers
;; these are mostly just wrappers around mpv's functions.

(fn event-hook [event function]
  "Set FUNCTION to be called when EVENT occurs.

See also: `event-unhook'"
  (mp.register_event event function))

(fn event-unhook [function]
  "Remove an event hook previously set with `event-hook'.

See also: `event-hook'"
  (mp.unregister_event function))

(fn observe [property function]
  "Set FUNCTION to be called when PROPERTY changes.

See also: `unobserve'"
  (mp.observe_property property "native" function))

(fn unobserve [function]
  "Remove an observer previously set with `observe'.

See also: `observe'"
  (mp.unobserve_property function))

;;; Media

(fn time-position []
  "Get the current time into the playing file."
  (mp.get_property_osd "time-pos"))

(fn time-total []
  "Get the total length of the playing file."
  (mp.get_property_osd "duration"))

(fn track-types []
  "Get a list of track types in the current file."
  (fcollect [idx 0 (- (mp.get_property_native "track-list/count") 1)]
    (mp.get_property_native (.. "track-list/" idx "/type"))))

(fn has-audio? []
  (contains? (track-types) "audio"))

(fn has-video? []
  (contains? (track-types) "video"))

(cmd frame-step-audio [n]
  "Step forward one frame worth of audio.

See also: `frame-back-step-audio'"
  (let [n (or n 1)]
    (do (mp.commandv "seek" (.. (/ n 60)) "exact") ; FIX: is it possible to play for this many seconds instead of just seeking?
        (mp.set_property_native "pause" true))))

(key-bind "." frame-step)

(cmd frame-back-step-audio [n]
  "Move backward N frames worth of audio.

See also: `frame-step-audio'"
  (let [n (or n 1)]
    (frame-step-audio (- n))))

(key-bind "," frame-back-step)

(fn frame-step-rebind [data]
  "Automatically rebind . and , to the correct \"frame step\" command when switching media: if the media has video, use the default command; otherwise, use `frame-step-audio'."
  (if (has-video?)
      (do
        (mp.remove_key_binding "frame-step-audio")
        (mp.remove_key_binding "frame-back-step-audio"))
      (do
        (mp.add_forced_key_binding "." "frame-step-audio" frame-step-audio {:repeatable true})
        (mp.add_forced_key_binding "," "frame-back-step-audio" frame-back-step-audio {:repeatable true}))))

(event-hook "file-loaded" frame-step-rebind) ; FIX: add an option to enable/disable this

;;; Playlist

(fn playlist-current-index []
  "Get the index of the currently-playing item in the playlist. Returns -1 if the playlist is empty or nothing is playing."
  (mp.get_property_native "playlist-pos"))

(fn playlist-length []
  "Get the length of the playlist."
  (mp.get_property_native "playlist-count"))

(fn playlist-goto-old [index]
  "Go to the specified item in the playlist, removing the target item if it no longer exists."
  (print "playlist-goto")
  (let [target-filename (mp.get_property_native (.. "playlist/" index "/filename"))]
    (if (not target-filename)
        (if (> index 0)
            (playlist-goto (- index 1))
            (message (.. "No such playlist entry: " index)))
        (if (not (file-exists? target-filename))
            (do (mp.commandv "playlist-remove" index)
                (playlist-goto index))
            (mp.set_property "playlist-pos" index)))))

(fn playlist-goto [index]
  "Go to the specified item in the playlist, removing the target item if it no longer exists."
  (let [target-filename (mp.get_property_native (.. "playlist/" index "/filename"))]
    (if (and target-filename
             (>= (- (playlist-length) 1) index 0))
        (if (and (not (path-remote? target-filename)) ; local paths might not be absolute, so path-local? would return false.
                 (not (file-exists? target-filename)))
            (do (mp.commandv "playlist-remove" index)
                (playlist-goto index))
            (mp.set_property "playlist-pos" index))
        (message (.. "No such playlist entry: " index)))))

(cmd playlist-next []
  "Go to the next item in the playlist, removing any nonexistent files in the way."
  (let [current (playlist-current-index)]
    (if (>= current (- (playlist-length) 1))
        (message "Already at end of playlist.")
        (playlist-goto (+ current 1)))))

(key-bind "n" playlist-next)

(cmd playlist-previous []
  "Go to the previous item in the playlist, removing any nonexistent files in the way."
  (let [current (playlist-current-index)]
    (if (= current 0)
        (message "Already at start of playlist.")
        (playlist-goto (- current 1)))))

(key-bind "p" playlist-previous)

(fn playlist-next-by-directory [direction]
  "Go to the next item in the playlist that is in a different directory, or skip to the start or end of the playlist if playlist_directory_skip_end is true."
  (let [direction (if (>= (or direction 1) 0) 1 -1)
        pos (playlist-current-index)
        last (if (>= direction 0)
                 (- (mp.get_property_native "playlist-count") 1)
                 0)
        cdir (path-directory (mp.get_property_native (.. "playlist/" pos "/filename")))]
    (var found false)
    (for [i pos last direction &until found]
      (when (not= cdir (path-directory (mp.get_property_native (.. "playlist/" i "/filename"))))
        (set found true)
        (playlist-goto i)))
    (when (not found)
      (if (. opts :playlist-directory-skip-end)
          (let [dir-str (if (>= direction 0) "end" "start")
                new-pos (if (>= direction 0) last 0)]
            (if (= new-pos pos)
                (message (.. "Already at playlist " dir-str "."))
                (do (message (.. "No other directory in this direction; skipping to playlist " dir-str "."))
                    (playlist-goto new-pos))))
          (message (.. "No item in the playlist outside of this directory when looking " (if (>= direction 0) "for" "back") "ward."))))))

(cmd playlist-next-directory []
  (playlist-next-by-directory 1))

(key-bind "N" playlist-next-directory)

(cmd playlist-previous-directory []
  (playlist-next-by-directory -1))

(key-bind "P" playlist-previous-directory)

(fn playlist-move [video-index new-index]
  "Move the video in the current playlist at VIDEO-INDEX to NEW-INDEX."
  (mp.command_native ["playlist-move" video-index new-index]))

(cmd playlist-move-forward [n]
  "Moves the current item in the playlist forward N items in the playlist."
  (let [n (or n 1)
        current-index (playlist-current-index)]
    (playlist-move current-index (+ n current-index))))

(cmd playlist-move-backward [n]
  "Moves the current item in the playlist backward N items in the playlist."
  (let [n (if n (- n) -1)]
    (playlist-move-forward n)))

(cmd playlist-dump []
  "Dump the playlist to a file."
  (let [filename (or filename "/home/modula/mpv-dumped-playlist.txt") ; FIX: turn the default into a setting
        count (mp.get_property_native "playlist-count")]
    (with-open [fout (io.open filename :w)]
      (for [idx 0 (- count 1)]
        (fout:write (.. (mp.get_property_native (.. "playlist/" idx "/filename")) "\n"))))
    (message (.. "Playlist dumped to " filename))))

(key-bind "Ctrl+\"" playlist-dump)

;;; Zoom/Pan

(cmd zoom-original []
  "Show the video in its original resolution, independent of the window size."
  (mp.set_property "video-unscaled" "yes"))

(cmd zoom-fill []
  "Stretch or shrink the video to fill as much of the window as possible, while keeping the correct aspect ratio."
  (mp.set_property "video-unscaled" "no"))

(cmd zoom-shrink []
  "Shrink the video to fit the window, or show it in its original resolution if it is smaller than the window."
  (mp.set_property "video-unscaled" "downscale-big"))

(fn window-size []
  "Get the dimensions of the MPV window as a list."
  (let [dim (mp.get_property_native "osd-dimensions")]
    [(. dim :w) (. dim :h)]))

(fn arrow-pan [x y]
  "Handle arrow key panning in the specified direction(s)."
  (let [x (or x 0)
        y (or y 0)]
    (print (.. "arrow-pan: " x " " y))
    (mp.commandv "add" "video-pan-x" (* x -1 (. opts :pan-amount)))
    (mp.commandv "add" "video-pan-y" (* y -1 (. opts :pan-amount)))))

(cmd arrow-left []
  "Pan leftward."
  (arrow-pan -1 0))

(cmd arrow-right []
  "Pan rightward."
  (arrow-pan 1 0))

(cmd arrow-up []
  "Pan upward."
  (arrow-pan 0 -1))

(cmd arrow-down []
  "Pan downward."
  (arrow-pan 0 1))

(key-bind "Alt+left" arrow-left {:repeatable true})
(key-bind "Alt+right" arrow-right {:repeatable true})
(key-bind "Alt+up" arrow-up {:repeatable true})
(key-bind "Alt+down" arrow-down {:repeatable true})

(fn set-pan-pixel [pixel-x pixel-y]
  "Set the video panning in pixels."
  (let [x-amount (* pixel-x (/ 1 (mp.get_property_native "width")))
        y-amount (* pixel-y (/ 1 (mp.get_property_native "height")))]
    (mp.set_property "video-pan-x" x-amount)
    (mp.set_property "video-pan-y" y-amount)))

(fn center []
  (mp.set_property "video-align-y" 0)
  (mp.set_property "video-pan-y" 0))

;;; Bar/OSD

(var status-bar-enabled false)

(fn draw-ass [ass]
  (let [(ww wh) (mp.get_osd_size)]
    (mp.set_osd_ass ww wh ass)))

(fn draw-text [ass text anchor x y text-size]
  "Draw TEXT to ASS at position X,Y with anchor point ANCHOR.

ANCHOR is a number from 1-9 inclusive, where the anchor points relative to the text are as per the following diagram:

7 8 9
4 5 6
1 2 3

Thus an anchor of 1 represents the bottom left point of the text, 5 represents the center, 9 represents the top right, etc."
  (let [expanded (mp.command_native ["expand-text" text])
        text-size (or text-size (. opts :status-bar-text-size))]
    (if (not expanded)
        (msg-warn "Error expanding status bar text.")
        (do ; (msg-verbose (.. "Status bar updated to: " expanded))
          (ass:new_event)
          (ass:an anchor)
          (ass:pos x y)
          (ass:append (.. "{\\fs" text-size "}{\\bord1.0}"))
          (ass:append expanded)))))

;; FIX: should the status-bar be registered as an "OSD" in mpv?
;; maybe we should specify the OSD margins by writing to the "user-data/osc/margins" property; https://mpv.io/manual/master/#command-interface-user-data/osc/margins
(fn status-bar-refresh []
  "Draw/refresh the status bar."
  ;; (msg-verbose "Refreshing status bar...")
  (let [ass (assdraw:ass_new)
        (w h) (mp.get_osd_size)
        margin (. opts :status-bar-margin)
        title-text-size (. opts :title-text-size)
        status-bar-text-size (. opts :status-bar-text-size)
        title (or (mp.get_property "title") "")
        path (or (mp.get_property_native "path") "")
        full-path (if (= "" path) "" (path-absolute path))
        size (or (mp.get_property_osd "file-size") "")
        width (mp.get_property_native "width") ; width of the media
        height (mp.get_property_native "height") ; height of the media
        dimensions (if (and width height)
                       (.. width "x" height)
                       "")
        play-pause (if (mp.get_property_native "core-idle") "⬛" "▶") ; ◼
        seek-time (time-position)
        total-time (time-total)
        zoom (or (mp.get_property_native "video-zoom") "")
        playlist-index (or (playlist-current-index) "")
        playlist-length (or (playlist-length) "")]
    ;; title
    (draw-text ass (.. title) 7 margin margin title-text-size)
    ;; left status
    (draw-text ass (.. full-path " | " size " | " dimensions) 1 margin (- h margin) status-bar-text-size)
    ;; right status
    (draw-text ass (.. play-pause " " seek-time "/" total-time " | " zoom "% | " playlist-index "/" playlist-length) 3 (- w margin) (- h margin) status-bar-text-size)
    (draw-ass ass.text)))

(cmd status-bar-enable []
  "Turn on display of the status bar."
  ;; (message "Status bar enabled")
  (set status-bar-enabled true)
  (mp.register_idle status-bar-refresh))

(cmd status-bar-disable []
  "Turn off display of the status bar."
  ;; (message "Status bar disabled")
  (set status-bar-enabled false)
  (mp.unregister_idle status-bar-refresh)
  (draw-ass ""))

(cmd status-bar-toggle [] ; FIX: make a hook for when the status bar is enabled/disabled, so that i can call minimap-toggle when the status bar is enabled and call it again when it's disabled
  "Toggle display of the status bar."
  (if status-bar-enabled
      (status-bar-disable)
      (status-bar-enable)))

(key-bind "b" status-bar-toggle)

(when (or (. opts :status-bar-enabled)
          (fennel.eval (. opts :status-bar-enable-when)))
  (status-bar-enable))

;;; Screenshots

(fn next-available-filename [name num]
  "Get the next available filename of NAME, where the string $NUM is replaced with NUM."
  (let [num (or num 0)]
    (var realpath (mp.command_native ["expand-text" name]))
    (print (.. "realpath: " realpath))
    (set realpath (mp.command_native ["expand-path" realpath]))
    (print (.. "realpath: " realpath))
    (set realpath (string.gsub realpath "%%NUM%%" (string.format "%04d" num)))
    (print (.. "realpath: " realpath))
    (if (file-exists? realpath)
        (next-available-filename name (+ num 1))
        realpath)))

(cmd take-screenshot [flags]
  "Take a screenshot and copy its filename to the clipboard."
  (let [flags (or flags "subtitles")
        path (path-no-extension (path-filename (mp.get_property_osd "path")))
        time-pos (time-position)
        next-filename (next-available-filename (. opts :screenshot-filename-format))]
    (mp.commandv "screenshot_to_file" next-filename flags)
    (copy-to-clipboard next-filename)
    (osd (.. "Screenshot: " next-filename))))

(cmd take-screenshot-no-subtitles []
  "Take a screenshot, like take-screenshot, but don't include subtitles."
  (take-screenshot "video"))

(key-bind "s" take-screenshot)
(key-bind "S" take-screenshot-no-subtitles)

;;; File Commands

(cmd copy-filename []
  (print (mp.get_property "path")) ; doesn't work in repl :(
  (let [path (path-absolute (mp.get_property "path"))]
    (copy-to-clipboard path)
    (message (.. "URL copied: " path))))

(key-bind "c" copy-filename)

(cmd delete-current-file []
  "Delete the current file from the filesystem and remove it from the playlist."
  (let [file (path-absolute (mp.get_property "path"))]
    (file-delete file)
    (let [pos (playlist-current-index)
          last (- (playlist-length) 1)]
      (when (= pos last)
        (playlist-goto (- pos 1)))
      (mp.commandv "playlist-remove" pos))))

(key-bind "D" delete-current-file)

(cmd open-directory [] ; FIX: don't require mpvi for this; also, come up with a better name that won't be confused with functionality like `show-media-parent'.
  "Open the current file's directory in a new mpv instance."
  (let [filename (path-absolute (mp.get_property "path"))
        ;; directory (path-directory (path-absolute (mp.get_property "path")))
        ]
    ;; (dprint (fennel.view (table-append ["mpv"]
    ;;                                    [(. opts :open-directory-arguments)]
    ;;                                    [directory])))
    (subprocess {:args ["mpvi" filename] :detach true})
    (message (.. "mpv " filename))))

(key-bind "v" open-directory)

(fn path-open-in-file-manager [directory file-manager]
  "Show DIRECTORY in the specified file manager. Defaults to the one set with the :file-manager option.

See also: `browse-url', `show-in-file-manager'"
  (let [file-manager (or file-manager (. opts :file-manager))]
    (subprocess {:args [file-manager directory] :detach true})
    (message (.. file-manager " " directory))))

(fn browse-path [path file-manager]
  "Show the directory containing PATH in the specified file manager. Defaults to the one set with the :file-manager option.

See also: `browse-url', `show-in-file-manager'"
  (path-open-in-file-manager (path-directory path) file-manager))

(cmd show-in-file-manager []
  "Show the current file in a file manager.

See also: `open-in-browser'"
  (browse-path (path-absolute (mp.get_property "path"))))

(fn browse-url [url browser]
  "Open URL in the specified browser. Defaults to the one set with the :browser option.

See also: `browse-path', `open-in-browser'"
  (let [browser (or browser (. opts :browser))]
    (subprocess {:args [browser url] :detach true})
    (message (.. browser " " url))))

(cmd open-in-browser []
  "Open the current file in a browser.

See also: `show-in-file-manager', `open-in-browser-at-time'"
  (browse-url (path-absolute (mp.get_property "path"))))

(key-bind "B" open-in-browser)

(cmd show-media-parent [] ; FIX: come up with a better name
  "Open the media's containing directory in a file manager, or its URL in a browser.

See also: `open-in-browser', `open-directory'"
  (let [path (mp.get_property "path")]
    (if (path-local? path)
        (show-in-file-manager)
        (open-in-browser))))

(key-bind "Alt+." show-media-parent)

(fn url-youtube-video? [url] ; FIX: sort elsewhere?
  "True if URL appears to be a YouTube video URL." ; FIX: check that the URL is actually a video URL.
  (and (url-protocol-http? url)
       (contains? ["www.youtube.com" "youtube.com" "youtu.be"] (url-domain url))))

(fn url-with-timecode [url time]
  "Get URL with an additional parameter to seek to TIME, if supported. Currently only YouTube URLs are supported."
  (let [url (or url (mp.get_property "path"))]
    (if (url-youtube-video? url)
        (let [time (or time (time-position))]
          (.. url "&t=" time))
        url)))

(cmd open-in-browser-at-time []
  "Open the current file in a browser, seeked to the current position if possible.

See also: `open-in-browser'"
  (browse-url (url-with-timecode (path-absolute (mp.get_property "path")))))

;;; Informational Commands

(cmd view-metadata []
  "Show metadata for the current file."
  (print (fennel.view (mp.get_property_native "metadata"))))

(key-bind "Alt+i" view-metadata)

;;; History

(fn history-record-this? []
  "True when the current media should be recorded to the history file."
  (let [exclusion-pattern (. opts :history-exclusion-pattern)
        path (path-absolute (mp.get_property "path"))]
    (and (. opts :record-history)
         (not (string.match path exclusion-pattern)))))

;; Based off of https://gist.github.com/garoto/e0eb539b210ee077c980e01fb2daef4a
(fn history-record [] ; FIX
  "Record the current media as an item in the history log."
  (when (history-record-this?)
    (let [log-filename (path-absolute (. opts :history-log-filename))]
      (when (not (utils.file_info log-filename))
        (subprocess {:args ["mkdir" (path-directory log-filename)] :detach false}))
      (let [filename (mp.get_property "filename")
            media-title (mp.get_property "media-title")
            write-title (if (= title filename) "" (string.format " (%s)" media-title))
            logfile (io.open log-filename "a+")]
        (logfile.write logfile (string.format "[%s] %s%s\n" (os.date "%d/%b/%y %X") (mp.get_property "path") write-title))
        (io.close logfile)))))

(event-hook "file-loaded" history-record)

;;; Miscellaneous

;; X11 screensaver heartbeat

(fn screensaver-running? []
  "True if org.freedesktop.ScreenSaver is available via dbus. This is used for `screensaver-heartbeat' in order to prevent the screensaver from starting if the screensaver-inhibit option is enabled."
  (dbus-service-available? "org.freedesktop.ScreenSaver"))

(fn screensaver-inhibit []
  "Send a dbus message to inhibit the screensaver via the org.freedesktop.ScreenSaver bus service."
  (subprocess {:args ["busctl" "--user" "call" "org.freedesktop.ScreenSaver" "/org/freedesktop/ScreenSaver" "org.freedesktop.ScreenSaver" "Inhibit" "ss" "multiplicative" "screensaver-inhibit"] :detach true}))

(fn xscreensaver-running? []
  "True if xscreensaver is running. This is used for `screensaver-heartbeat' in order to prevent the screensaver from starting if the screensaver-inhibit option is enabled."
  (> (length (system-processes-matching "xscreensaver")) 0))

(fn xscreensaver-inhibit []
  "Send a dbus message to inhibit the screensaver via the org.freedesktop.ScreenSaver bus service."
  (subprocess {:args ["xscreensaver-command" "--deactivate"] :detach true}))

(fn blank-inhibit []
  "Call xset to inhibit screen blanking."
  (subprocess {:args ["xset" "s" "reset"] :detach true}))

(var screensaver-enabled? (and (. opts :screensaver-inhibit)
                               (screensaver-running?)))

(var xscreensaver-enabled? (and (. opts :screensaver-inhibit)
                                (xscreensaver-running?)))

(fn screensaver-heartbeat []
  "Ensure xscreensaver does not run if the media is playing and has an audio track. This will automatically be called every 30 seconds if the screensaver-inhibit option is enabled."
  (let [paused (mp.get_property_native "pause")
        muted (or (mp.get_property_native "mute")
                  (= 0 (mp.get_property_native "volume")))]
    (when (and (not paused) ; send the heartbeat if we're not paused
               (has-audio?) ; and the media has audio (i.e. if it's not a gif)
               (not muted)) ; and the audio is unmuted
      (when screensaver-enabled?
        (screensaver-inhibit))
      (when xscreensaver-enabled?
        (xscreensaver-inhibit))
      (blank-inhibit))))

(when (. opts :screensaver-inhibit)
  (mp.add_periodic_timer 30 screensaver-heartbeat))

;; exit after end delay

(fn playlist-ended? []
  "True if we are paused at the end of the last file."
  (mp.get_property_native "eof-reached"))

(var pause-exit-timer nil)

(fn stop-pause-exit-timer []
  (when pause-exit-timer
    (pause-exit-timer.stop)))

(fn exit-if-ended []
  "Exit mpv if we're still paused and at the end of the last file. Called by the timer started by on-pause."
  (if (playlist-ended?)
      (mp.command "quit")
      (stop-pause-exit-timer)))

(fn on-pause [property paused]
  "Check if we are paused at the end of the last playlist item, and if so, start the exit timer."
  (if (and paused (playlist-ended?))
      (let [sec (. opts :pre-exit-pause-time)]
        (message (.. "End of playlist; exiting in " sec "s..."))
        (set pause-exit-timer (mp.add_timeout sec exit-if-ended)))
      (stop-pause-exit-timer)))

(when (. opts :pre-exit-pause-time)
  (observe "pause" on-pause))

;; clean up socket file on exit

(fn delete-socket []
  "Delete the socket file."
  (let [socket-file (mp.get_property "input-ipc-server")]
    (when (not= "" socket-file)
      (file-delete socket-file))))

(event-hook "shutdown" delete-socket)

;;; scratch

(cmd debug-command []
  (print (.. "mute: " (fennel.view (mp.get_property_native "mute"))))
  (print (.. "volume: " (mp.get_property_native "volume")))
  (let [paused (mp.get_property_native "pause")
        muted (or (mp.get_property_native "mute")
                  (= 0 (mp.get_property_native "volume")))]
    (print (.. "not paused: " (fennel.view (not paused)) " has-audio: " (fennel.view (has-audio?)) " not muted: " (fennel.view (not muted)))))
  (print (fennel.view (window-size)))
  (print (.. "DISPLAY: " (os.getenv "DISPLAY")))
  (each [key value (pairs _G)]
    (print (.. "--- " key " ---"))
    (print (fennel.view value))))

(key-bind "X" debug-command)

(local repl-input-buffer []) ; incomplete inputs
(local repl-values-buffer []) ; results of the evaluated code
(var repl-running? false)
(var repl-input-incomplete? false)

(fn repl-print [outputs]
  (print "repl-print")
  (print (fennel.view outputs))
  (each [index output (ipairs outputs)]
    (print (.. "Output: " output))
    ;; (ass:append (.. "{\\fs" (. opts :status-bar-text-size) "}{\\bord1.0}"))
    (input.log (.. "Output: " output) ))) ; FIX: style the outputs

(fn repl-values [results]
  (print "repl-values")
  (print (fennel.view results))
  (each [index result (ipairs results)]
    (print (.. "Result " index ": " result))
    (input.log (.. "Result " index ": " result) ) ; FIX: style the results
    (table.insert repl-values-buffer result)))

;; FIX: use fennel.traceback (instead of the default debug.traceback) for fennel's stack traces instead of lua's.
(fn repl-error [error-type msg lua-source]
  (print "repl-error")
  (print (fennel.view error-type))
  (print (fennel.view msg))
  (print (.. error-type " Error: " msg))
  (input.log_error (.. error-type " Error: " msg))
  ;; (each [line (msg:gmatch "([^\n]+)")]
  ;;   (print (.. "Error: " line))
  ;;   (input.log_error (.. "Error: " line))
  ;;   ;; (table.insert repl-values-buffer [[0.9 0.4 0.5] line])
  ;;   )
  )

(global tpr (fn tpr [one]
              (print "tpr")
              (print one)))

(global tpr-2 (fn tpr-2 [one]
                (print "tpr-2")
                (print one)))

(set (. _G "tpr-2") tpr-2)

(local env (table-kvappend _G
                           {: string :print #(repl-print (icollect [_ x (ipairs [$...])] ; override print so that its output can be captured
                                                           (tostring x)))}))

(local repl-coroutine (coroutine.create fennel.repl.repl))

(coroutine.resume repl-coroutine {:readChunk coroutine.yield
                                  :onValues repl-values
                                  :onError repl-error
                                  ;; :allowedGlobals [print]
                                  :assertAsRepl true
                                  :env env
                                  :compilerEnv env})

(fn repl-process []
  "Process the repl-input-buffer and evaluate the input if it's complete."
  (print "repl-process")
  (let [input-text (table.concat repl-input-buffer)]
    (print (.. "Eval: " input-text))
    (input.log (.. "Eval: " input-text))
    (let [input-text (table.concat (doto repl-input-buffer (table.insert "\n")))
          (_ {: stack-size}) (coroutine.resume repl-coroutine input-text)]
      (set repl-input-incomplete? (< 0 stack-size))
      (print (.. "repl-input-incomplete? " (fennel.view repl-input-incomplete?)))))
  (while (next repl-input-buffer) ; empty the input table
    (table.remove repl-input-buffer)))

(cmd repl-enter [text]
  "Process a line of text input into the REPL."
  (print "repl-enter")
  (print text)
  (table.insert repl-input-buffer text)
  (repl-process))

(cmd repl-exit [text cursor-pos]
  "Close the REPL."
  (print "repl-exit")
  (set repl-running? false))

(cmd repl []
  "Start the REPL."
  (print "repl")
  (set repl-running? true)
  (input.get {:prompt (if repl-input-incomplete? "... : " "Eval: ")
              :submit repl-enter
              :closed repl-exit
              :id "multiplicative-repl"}))

(key-bind ":" repl)
(key-bind "Alt+:" repl)
