import pluginJs from "@eslint/js";
import globals from "globals";

/** @type {import("eslint").Linter.FlatConfig[]} */
export default [
  // 1. Base recommended config
  pluginJs.configs.recommended,

  // 2. Default config for all JS files (except those specifically overridden)
  {
    files: ["**/*.js"],
    languageOptions: {
      sourceType: "script",
      globals: {
        ...globals.browser,
        // Singletons as readonly globals everywhere except
        // where they’re actually defined
        Bespoke: "readonly",
        AddAttachments: "readonly",
        Auth: "readonly",
        TopPosts: "readonly",
        AddReplyPost: "readonly",
        // Global libraries etc
        uhtml: "readonly",
        UIkit: "readonly",
        marked: "readonly",
        sodium: "readonly",
        importScripts: "readonly",
      },
    },
  },

  // 3. Override for files that actually *define* the singletons
  {
      files: ["bespoke.js", "add_attachments.js", "auth.js", "add_reply_post.js", "post.js", "top_posts.js"],
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
