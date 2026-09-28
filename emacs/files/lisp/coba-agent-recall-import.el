;;; coba-agent-recall-import.el --- One-shot native session import -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file manually, then run
;; `coba-agent-recall-import-native-sessions'.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun coba-agent-recall-import-native-sessions ()
  "Import native Claude and Codex sessions as agent-recall stubs.

The provider histories remain authoritative.  This command creates only
small Markdown files containing the metadata agent-recall needs to list
and resume a session.  Existing transcripts are never overwritten."
  (interactive)
  (require 'agent-recall)
  (require 'json)
  (let* ((transcript-root
          (expand-file-name
           (or (car agent-recall-search-paths)
               "~/.emacs.d/agent-shell")))
         (claude-root (expand-file-name "~/.claude/projects"))
         (codex-root (expand-file-name "~/.codex"))
         (records (make-hash-table :test #'equal))
         (codex-labels (make-hash-table :test #'equal))
         (known-sessions (make-hash-table :test #'equal))
         (created 0)
         (existing 0)
         (labels 0))
    (cl-labels
        ((read-json-line
           (line)
           (condition-case nil
               (json-parse-string line
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil)
             (error nil)))
         (clean-text
           (text)
           (when (stringp text)
             (truncate-string-to-width
              (string-trim
               (replace-regexp-in-string "[[:space:]\n\r]+" " " text))
              500 nil nil t)))
         (iso-time
           (value)
           (when (and (stringp value)
                      (not (string-empty-p value)))
             (condition-case nil (date-to-time value)
               (error nil))))
         (register
           (provider id cwd started modified preview)
           (when (and (stringp id)
                      (not (string-empty-p id))
                      (stringp cwd)
                      (not (string-empty-p cwd)))
             (puthash id
                      (list
                       :provider provider
                       :cwd cwd
                       :started (or started modified
                                    (current-time))
                       :modified (or modified started
                                     (current-time))
                       :preview (or (clean-text preview)
                                    "(no preview)"))
                      records)))
         (scan-lines
           (file function)
           (condition-case nil
               (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (while (not (eobp))
                   (when-let* ((data
                                (read-json-line
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
                     (funcall function data))
                   (forward-line 1)))
             (error nil)))
         (write-stub
           (id record)
           (let* ((cwd (plist-get record :cwd))
                  (provider (plist-get record :provider))
                  (started (plist-get record :started))
                  (modified (plist-get record :modified))
                  (project (file-name-nondirectory
                            (directory-file-name cwd)))
                  (directory
                   (expand-file-name
                    (format "%s/.agent-shell/transcripts" project)
                    transcript-root))
                  (file
                   (expand-file-name
                    (format "%s-native-%s-%s.md"
                            (format-time-string "%F-%H-%M-%S"
                                                started)
                            (replace-regexp-in-string
                             "[^[:alnum:]]+" "-" (downcase provider))
                            id)
                    directory)))
             (if (gethash id known-sessions)
                 (cl-incf existing)
               (make-directory directory t)
               (with-temp-file file
                 (insert
                  (format
                   (concat
                    "<!-- coba-agent-recall-native-stub -->\n"
                    "# Agent Shell Transcript\n\n"
                    "**Agent:** %s\n"
                    "**Started:** %s\n"
                    "**Working Directory:** %s\n"
                    "**Session ID:** %s\n\n"
                    "---\n\n## User\n\n> %s\n")
                   provider
                   (format-time-string "%F %T" started)
                   cwd id (plist-get record :preview))))
               (set-file-times file modified)
               (puthash id file known-sessions)
               (cl-incf created)))))
      ;; Avoid duplicates for sessions which already have agent-shell
      ;; transcripts, including stubs created by an earlier import.
      (when (file-directory-p transcript-root)
        (dolist (file (directory-files-recursively
                       transcript-root "\\.\\(?:md\\|org\\)\\'"))
          (when-let* ((id (agent-recall--read-embedded-session-id
                           file)))
            (puthash id file known-sessions))))
      ;; Claude's indexes contain the best title, project, and timestamp
      ;; metadata.  Raw JSONL scanning below fills in sessions absent from
      ;; an index.
      (when (file-directory-p claude-root)
        (dolist (index (directory-files-recursively
                        claude-root "sessions-index\\.json\\'"))
          (condition-case nil
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (data (json-read-file index)))
                (dolist (entry (alist-get 'entries data))
                  (unless (alist-get 'isSidechain entry)
                    (register
                     "Claude Code"
                     (alist-get 'sessionId entry)
                     (alist-get 'projectPath entry)
                     (iso-time (alist-get 'created entry))
                     (iso-time (alist-get 'modified entry))
                     (or (alist-get 'summary entry)
                         (alist-get 'firstPrompt entry))))))
            (error nil)))
        (dolist (file (directory-files-recursively claude-root
                                                   "\\.jsonl\\'"))
          (let (first-user)
            (scan-lines
             file
             (lambda (data)
               (when (and (not first-user)
                          (equal (alist-get 'type data) "user")
                          (not (alist-get 'isSidechain data)))
                 (setq first-user t)
                 (let* ((message (alist-get 'message data))
                        (content (alist-get 'content message)))
                   (register
                    "Claude Code"
                    (alist-get 'sessionId data)
                    (alist-get 'cwd data)
                    (iso-time (alist-get 'timestamp data))
                    (file-attribute-modification-time
                     (file-attributes file))
                    (if (stringp content) content nil)))))))))
      ;; codex-ide's rename-buffer command persists names through
      ;; thread/name/set; Codex mirrors them in session_index.jsonl.
      (let ((index (expand-file-name "session_index.jsonl"
                                     codex-root)))
        (when (file-readable-p index)
          (scan-lines
           index
           (lambda (data)
             (when-let* ((id (alist-get 'id data))
                         (name (alist-get 'thread_name data)))
               (puthash id name codex-labels))))))
      (let ((sessions-root (expand-file-name "sessions" codex-root)))
        (when (file-directory-p sessions-root)
          (dolist (file (directory-files-recursively
                         sessions-root "\\.jsonl\\'"))
            (let (metadata preview)
              (scan-lines
               file
               (lambda (data)
                 (pcase (alist-get 'type data)
                   ("session_meta"
                    (setq metadata (alist-get 'payload data)))
                   ("event_msg"
                    (let ((payload (alist-get 'payload data)))
                      (when (and (not preview)
                                 (equal (alist-get 'type payload)
                                        "user_message"))
                        (setq preview (alist-get 'message payload))))))))
              (when-let* ((id (alist-get 'id metadata)))
                (let ((source (alist-get 'source metadata)))
                  (unless (or (alist-get 'parent_thread_id metadata)
                              (and (listp source)
                                   (alist-get 'subagent source)))
                    (register
                     "Codex" id (alist-get 'cwd metadata)
                     (iso-time (alist-get 'timestamp metadata))
                     (file-attribute-modification-time
                      (file-attributes file))
                     (or (gethash id codex-labels) preview)))))))))
      (maphash #'write-stub records)
      (maphash
       (lambda (id name)
         (when (gethash id records)
           (agent-recall-metadata-put id 'label name)
           (cl-incf labels)))
       codex-labels)
      (agent-recall-reindex)
      (message
       "Imported %d native sessions; %d already indexed; restored %d Codex names"
       created existing labels))))

(provide 'coba-agent-recall-import)
;;; coba-agent-recall-import.el ends here
