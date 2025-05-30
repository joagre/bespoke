import pluginJs from "@eslint/js";
import globals from "globals";

/** @type {import("eslint").Linter.FlatConfig[]} */
export default [
  // 1. Base recommended config
  pluginJs.configs.recommended,

  // 2. Default config for all JS files (except for "is not defined")
  {
    files: ["**/*.js"],
    languageOptions: {
      sourceType: "script",

      globals: {
        ...globals.browser,
        // Bespoke singleton libraries
        About: "readonly",
        Bespoke: "readonly",
        Crypto: "readonly",
        FileDB: "readonly",
        HasBeenReadDivider: "readonly",
        MessageLib: "readonly",
        PostLib: "readonly",
        Progress: "readonly",
        ScrollNavigator: "readonly",
        streamSaver: "readonly",
        // Third-party singleton libraries
        DOMPurify: "readonly",
        importScripts: "readonly",
        marked: "readonly",
        sodium: "readonly",
        uhtml: "readonly",
        UIkit: "readonly",
        // Bespoke page singletons used as libraries
        AddAttachments: "readonly",
        AddReplyMessage: "readonly",
        AddReplyPost: "readonly",
        AddTopMessage: "readonly",
        Settings: "readonly",
        StickyHeader: "readonly",
        TopMessages: "readonly",
        TopPosts: "readonly"
      },
    },
    rules: {
      "no-unused-vars": [
        "error",
        {
          varsIgnorePattern: "^_",
          argsIgnorePattern: "^_"
        }
      ]
    }
  },

  // 3. Override for files that actually *define* singletons
  {
    files: [
      "about.html",
      "add_attachments.js",
      "add_reply_message.js",
      "add_reply_post.js",
      "add_top_message.js",
      "add_top_post.js",
      "bespoke.js",
      "bootstrap.js",
      "change_password.js",
      "crypto.js",
      "file_db.js",
      "files.js",
      "has_been_read_divider.js",
      "index.js",
      "loader.js",
      "login.js",
      "message.js",
      "message_lib.js",
      "post.js",
      "post_lib.js",
      "scroll_navigator.js",
      "settings.js",
      "switch_user.js",
      "sticky_header.js",
      "top_messages.js",
      "top_posts.js",
      "progress.js"
    ],
    languageOptions: {
      sourceType: "script",
      globals: {
        ...globals.browser
      },
    },
    rules: {
      // Let us redeclare these singletons locally
      "no-redeclare": "off",
      // Don’t complain if we define them but don’t immediately use them
      "no-unused-vars": "off",
    },
  },
];
