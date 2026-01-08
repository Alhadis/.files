/**
 * @fileoverview
 *   Configuration for my main Firefox profile. Loaded at startup after “prefs.js”,
 *   overriding anything declared there. Note that this latter file is regenerated
 *   each time Firefox preferences are modified, making it impossible to document a
 *   subset of meaningful browser settings like those listed herein.
 *
 * @see {@link view-source:chrome://browser/content/browser.xhtml|Firefox UI skeleton}
 * @sources Profile directories:
 *   [User profiles overview]{@link https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data}
 *   [Modifying config files]{@link https://kb.mozillazine.org/Editing_configuration}
 *
 * @sources Setting reference:
 *   [Preference definitions]{@link https://searchfox.org/mozilla-central/source/browser/app/profile/firefox.js}
 *   [Preference file syntax]{@link https://searchfox.org/mozilla-central/source/modules/libpref/parser/src/lib.rs}
 *   [Static preference list]{@link https://searchfox.org/mozilla-central/source/modules/libpref/init/StaticPrefList.yaml}
 *   [Unstable feature flags]{@link https://developer.mozilla.org/en-US/docs/Mozilla/Firefox/Experimental_features}
 *   [Preference parser docs]{@link https://firefox-source-docs.mozilla.org/modules/libpref/index.html}
 */

// Enable site-specific CSS tweaks
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Homepage and startup behaviour
user_pref("browser.startup.page", 3);
user_pref("browser.startup.homepage", "https://isrupertmurdochdead.com/");
user_pref("browser.newtabpage.enabled", false);

// Skip the “first run” jazz
user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.shell.checkDefaultBrowser", true);
user_pref("browser.engagement.ctrlTab.has-used", true);
user_pref("devtools.everOpened", true);
user_pref("devtools.selfxss.count", 5);
user_pref("devtools.webconsole.input.editorOnboarding", false);
user_pref("extensions.recommendations.hideNotice", true);
user_pref("devtools.inspector.simple-highlighters.message-dismissed", true);
user_pref("devtools.performance.new-panel-onboarding", false);
user_pref("devtools.performance.popup.intro-displayed", true);

// Tabbed navigation and window behaviour
user_pref("browser.ctrlTab.sortByRecentlyUsed", false);
user_pref("browser.urlbar.contextualSearch.enabled", false);
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.tabs.groups.enabled", true);
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.tabs.groups.dragOverDelayMS", 150);
user_pref("browser.tabs.warnOnOpen", false);
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.warnOnQuitShortcut", false);
user_pref("browser.tabs.context.close-duplicate.enabled", true);
user_pref("browser.tabs.hoverPreview.enabled", false);
user_pref("browser.tabs.hoverPreview.showThumbnails", false);
user_pref("browser.tabs.fadeOutUnloadedTabs", true);
user_pref("browser.tabs.inTitlebar", 1);
user_pref("general.autoScroll", true);
user_pref("general.smoothScroll", true);

// Weather forecasts
user_pref("browser.urlbar.suggest.weather", true);
user_pref("browser.urlbar.weather.featureGate", true);
user_pref("browser.urlbar.weather.minKeywordLength", 3);
user_pref("browser.urlbar.weather.uiTreatment", 2);

// Search-bar
user_pref("browser.search.suggest.enabled", true);
user_pref("browser.search.update", false);
user_pref("browser.search.region", "AU");
user_pref("browser.urlbar.ctrlCanonizesURLs", false);
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.quicksuggest.mlEnabled", false);
user_pref("browser.urlbar.quicksuggest.online.available", false);
user_pref("browser.urlbar.quicksuggest.online.enabled", false);
user_pref("browser.urlbar.shortcuts.actions", true);
user_pref("browser.urlbar.shortcuts.bookmarks", false);
user_pref("browser.urlbar.shortcuts.history", false);
user_pref("browser.urlbar.shortcuts.tabs", false);
user_pref("browser.urlbar.showSearchTerms.enabled", false);
user_pref("browser.urlbar.showSearchTerms.featureGate", false);
user_pref("browser.urlbar.showSearchSuggestionsFirst", false);
user_pref("browser.urlbar.speculativeConnect.enabled", false);
user_pref("browser.urlbar.suggest.bookmark", true);
user_pref("browser.urlbar.suggest.calculator", false);
user_pref("browser.urlbar.suggest.clipboard", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("browser.urlbar.suggest.history", true);
user_pref("browser.urlbar.suggest.openpage", true);
user_pref("browser.urlbar.suggest.quickactions", false);
user_pref("browser.urlbar.suggest.recentsearches", false);
user_pref("browser.urlbar.suggest.searches", false);
user_pref("browser.urlbar.suggest.topsites", true);
user_pref("browser.urlbar.suggest.trending", false);
user_pref("browser.urlbar.trending.featureGate", false);
user_pref("browser.urlbar.unitConversion.enabled", false);
user_pref("browser.urlbar.update2.engineAliasRefresh", true);

// Bookmarks
user_pref("browser.toolbars.bookmarks.showOtherBookmarks", false);
user_pref("browser.toolbars.bookmarks.visibility", "always");
user_pref("sidebar.animation.enabled", false);
user_pref("sidebar.main.tools", "bookmarks,history");
user_pref("sidebar.revamp", false);
user_pref("sidebar.verticalTabs", false);
user_pref("sidebar.visibility", "always-show");

// Theme (TODO: Support translucent windows and native macOS themes)
user_pref("browser.display.document_color_use", 0);
user_pref("browser.theme.content-theme", 2);
user_pref("browser.theme.toolbar-theme", 2);
user_pref("browser.theme.native-theme", false);
user_pref("browser.theme.macos.native-theme", false);
user_pref("browser.tabs.allow_transparent_browser", false);
user_pref("extensions.activeThemeID", "default-theme@mozilla.org");

// Extensions
user_pref("extensions.blocklist.level", 2);
user_pref("extensions.pocket.enabled", false);
user_pref("extensions.update.enabled", true);
user_pref("extensions.update.autoUpdateDefault", true);
user_pref("extensions.ui.dictionary.hidden", false);
user_pref("extensions.ui.lastCategory", "addons://list/extension");
user_pref("extensions.ui.locale.hidden", false);
user_pref("extensions.ui.sitepermission.hidden", false);

// Localisation and spellcheck
user_pref("intl.accept_languages", "en-au,en-gb,en");
user_pref("intl.locale.requested", "en-GB,en-US");
user_pref("intl.regional_prefs.use_os_locales", true);
user_pref("layout.spellcheckDefault", 1);

// Forms
user_pref("browser.formfill.enable", false);
user_pref("dom.forms.datetime.others", true);
user_pref("dom.forms.datetime.timepicker", true);
user_pref("dom.forms.selectSearch", true);
user_pref("dom.input.showPicker_datalist.enabled", true);
user_pref("dom.input.number_and_range_modified_by_mousewheel", false);
user_pref("extensions.formautofill.addresses.enabled", false);
user_pref("extensions.formautofill.addresses.capture.enabled", false);
user_pref("extensions.formautofill.addresses.experiments.enabled", false);
user_pref("extensions.formautofill.addresses.supportedCountries", "");
user_pref("extensions.formautofill.addresses.supported", "off");
user_pref("extensions.formautofill.creditCards.enabled", false);
user_pref("extensions.formautofill.creditCards.supported", "off");
user_pref("extensions.formautofill.creditCards.supportedCountries", "");
user_pref("signon.autofillForms", true);
user_pref("signon.rememberSignons", true);
user_pref("signon.firefoxRelay.feature", "disabled");

// Privacy and security
user_pref("browser.privatebrowsing.autostart", false);
user_pref("browser.safebrowsing.downloads.enabled", true);
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", true);
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", true);
user_pref("browser.safebrowsing.malware.enabled", true);
user_pref("browser.safebrowsing.phishing.enabled", true);
user_pref("dom.disable_open_during_load", false);
user_pref("dom.private-attribution.submission.enabled", true);
user_pref("dom.security.https_only_mode", false);
user_pref("dom.security.https_only_mode_pbm", false);
user_pref("privacy.donottrackheader.enabled", true);
user_pref("privacy.globalprivacycontrol.enabled", true);
user_pref("privacy.globalprivacycontrol.was_ever_enabled", true);
user_pref("privacy.history.custom", false);
user_pref("security.enterprise_roots.enabled", true);
user_pref("security.fileuri.strict_origin_policy", true);
user_pref("security.OCSP.enabled", 1);
user_pref("signon.management.page.breach-alerts.enabled", true);
user_pref("signon.management.page.vulnerable-passwords.enabled", true);

// Telemetry
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);
user_pref("browser.discovery.enabled", true);
user_pref("datareporting.healthreport.uploadEnabled", true);
user_pref("datareporting.usage.uploadEnabled", true);

// Disable “Containers” feature
user_pref("privacy.userContext.enabled", false);
user_pref("privacy.userContext.ui.enabled", false);
user_pref("privacy.userContext.extension", "");

// Reset concessions made for deprecated HTTPS versions
user_pref("security.tls.version.enable-deprecated", false);

// Keep downloaded files out of browsing history
user_pref("browser.download.manager.addToRecentDocs", false);
user_pref("browser.download.clearHistoryOnDelete", 2);

// Tell “AI” to fuck off, and ensure it stays fucked-off
user_pref("browser.search.visualSearch.featureGate", false);
user_pref("browser.ml.enable", false);
user_pref("browser.ml.chat.menu", false);
user_pref("browser.ml.chat.page", false);
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.ml.chat.sidebar", false);
user_pref("browser.ml.chat.shortcuts", false);
user_pref("browser.ml.chat.provider", "");
user_pref("browser.ml.linkPreview.enabled", false);
user_pref("browser.ml.linkPreview.longPress", false);
user_pref("browser.ml.modelCacheMaxSize", 0);
user_pref("extensions.htmlaboutaddons.local_model_management", false);
user_pref("extensions.ml.enabled", false);

// Declutter new-tab windows
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.system.topstories",  false);
user_pref("browser.newtabpage.activity-stream.showSponsoredCheckboxes",  false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites",    false);

// Don't suggest extensions or new features
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);

// Picture-in-Picture display
user_pref("media.videocontrols.picture-in-picture.enabled", true);
user_pref("media.videocontrols.picture-in-picture.audio-toggle.enabled", false);
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", true);
user_pref("media.videocontrols.picture-in-picture.enable-when-switching-tabs.enabled", true);
user_pref("media.videocontrols.picture-in-picture.keyboard-controls.enabled", true);
user_pref("media.videocontrols.picture-in-picture.urlbar-button.enabled", false);

// Enable experimental CSS and JavaScript features
user_pref("layout.css.anchor-positioning.enabled", true);
user_pref("layout.css.at-scope.enabled", true);
user_pref("layout.css.basic-shape-shape.enabled", true);
user_pref("layout.css.control-characters.visible", true);
user_pref("layout.css.convertFromNode.enable", true);
user_pref("layout.css.cross-fade.enabled", true);
user_pref("layout.css.field-sizing.enabled", true);
user_pref("layout.css.font-variant-emoji.enabled", true);
user_pref("layout.css.getBoxQuads.enabled", true);
user_pref("layout.css.grid-template-masonry-value.enabled", true);
user_pref("layout.css.has-slotted-selector.enabled", true);
user_pref("layout.css.initial-letter.enabled", true);
user_pref("layout.css.inverted-colors.enabled", true);
user_pref("layout.css.modern-range-pseudos.enabled", true);
user_pref("layout.css.module-scripts.enabled", true);
user_pref("layout.css.moz-broken.content.enabled", true);
user_pref("layout.css.moz-control-character-visibility.enabled", true);
user_pref("layout.css.prefers-reduced-transparency.enabled", true);
user_pref("layout.css.scroll-driven-animations.enabled", true);
user_pref("layout.css.zoom.enabled", true);
user_pref("javascript.options.experimental.iterator_helpers", true);
user_pref("javascript.options.experimental.json_parse_with_source", true);
user_pref("javascript.options.experimental.regexp_modifiers", true);
user_pref("javascript.options.experimental.shadow_realms", true);
user_pref("javascript.options.experimental.weakrefs.expose_cleanupSome", true);
user_pref("dom.capture.enabled", true);
user_pref("dom.css_pseudo_element.enabled", true);
user_pref("dom.customHighlightAPI.enabled", true);
user_pref("dom.element.blocking.enabled", true);
user_pref("dom.element.contenteditable.plaintext-only.enabled", true);
user_pref("dom.enable_memory_stats", true);
user_pref("dom.enable_web_task_scheduling", true);
user_pref("dom.file.createInChild", true);
user_pref("dom.paintWorklet.enabled", true);
user_pref("dom.screenorientation.allow-lock", true);
user_pref("dom.text-recognition.enabled", false);
user_pref("dom.viewTransitions.enabled", true);
user_pref("dom.webgpu.enabled", true);
user_pref("dom.webshare.enabled", true);
user_pref("media.rvfc.enabled", true);
user_pref("media.seekToNextFrame.enabled", true);
user_pref("media.webrtc.hw.h264.enabled", true);

// Developer tools
user_pref("devtools.browserconsole.filter.css", true);
user_pref("devtools.browserconsole.filter.debug", true);
user_pref("devtools.browserconsole.filter.error", true);
user_pref("devtools.browserconsole.filter.info", true);
user_pref("devtools.browserconsole.filter.log", true);
user_pref("devtools.browserconsole.filter.net", true);
user_pref("devtools.browserconsole.filter.netxhr", true);
user_pref("devtools.browserconsole.filter.warn", true);
user_pref("devtools.browserconsole.input.editor", true);
user_pref("devtools.browserconsole.input.editorWidth", 300);
user_pref("devtools.cache.disabled", true);
user_pref("devtools.chrome.enabled", true);
user_pref("devtools.command-button-errorcount.enabled", true);
user_pref("devtools.command-button-frames.enabled", true);
user_pref("devtools.command-button-measure.enabled", true);
user_pref("devtools.command-button-pick.enabled", true);
user_pref("devtools.command-button-responsive.enabled", true);
user_pref("devtools.command-button-rulers.enabled", true);
user_pref("devtools.command-button-screenshot.enabled", true);
user_pref("devtools.debugger.map-scopes-enabled", true);
user_pref("devtools.defaultColorUnit", "hex");
user_pref("devtools.dom.enabled", true);
user_pref("devtools.editor.autoclosebrackets", true);
user_pref("devtools.editor.autocomplete", true);
user_pref("devtools.editor.detectindentation", true);
user_pref("devtools.editor.expandtab", false);
user_pref("devtools.editor.keymap", "default");
user_pref("devtools.editor.tabsize", 4);
user_pref("devtools.eyedropper.zoom", 10);
user_pref("devtools.f12_enabled", true);
user_pref("devtools.inspector.draggable_properties", false);
user_pref("devtools.inspector.rule-view.focusNextOnEnter", false);
user_pref("devtools.inspector.show_pseudo_elements", false);
user_pref("devtools.inspector.showAllAnonymousContent", true);
user_pref("devtools.inspector.showUserAgentStyles", true);
user_pref("devtools.layout.boxmodel.highlightProperty", true);
user_pref("devtools.layout.boxmodel.opened", true);
user_pref("devtools.layout.flex-container.opened", false);
user_pref("devtools.layout.flex-item.opened", false);
user_pref("devtools.layout.flexbox.opened", false);
user_pref("devtools.layout.grid.opened", false);
user_pref("devtools.markup.beautifyOnCopy", true);
user_pref("devtools.markup.collapseAttributes", true);
user_pref("devtools.markup.collapseAttributeLength", 300);
user_pref("devtools.overflow.debugging.enabled", true);
user_pref("devtools.responsive.leftAlignViewport.enabled", false);
user_pref("devtools.responsive.reloadNotification.enabled", false);
user_pref("devtools.responsive.touchSimulation.enabled", true);
user_pref("devtools.responsive.viewport.angle", 90);
user_pref("devtools.responsive.viewport.pixelRatio", 2);
user_pref("devtools.source-map.client-service.enabled", true);
user_pref("devtools.styleeditor.atRulesSidebarWidth", 300);
user_pref("devtools.styleeditor.autocompletion-enabled", true);
user_pref("devtools.styleeditor.enabled", true);
user_pref("devtools.styleeditor.navSidebarWidth", 350);
user_pref("devtools.styleeditor.showAtRulesSidebar", true);
user_pref("devtools.styleeditor.transitions", true);
user_pref("devtools.toolbox.alwaysOnTop", true);
user_pref("devtools.toolbox.host", "bottom");
user_pref("devtools.toolbox.previousHost", "window");
user_pref("devtools.toolbox.selectedTool", "webconsole");
user_pref("devtools.toolbox.splitconsole.enabled", true);
user_pref("devtools.toolbox.splitconsole.open", false);
user_pref("devtools.toolbox.tabsOrder", "inspector,webconsole,jsdebugger,styleeditor,netmonitor,accessibility");
user_pref("devtools.toolbox.zoomValue", 1);
user_pref("devtools.webconsole.filter.css", true);
user_pref("devtools.webconsole.filter.debug", false);
user_pref("devtools.webconsole.filter.error", true);
user_pref("devtools.webconsole.filter.info", true);
user_pref("devtools.webconsole.filter.log", true);
user_pref("devtools.webconsole.filter.net", true);
user_pref("devtools.webconsole.filter.netxhr", true);
user_pref("devtools.webconsole.filter.warn", true);
user_pref("devtools.webconsole.groupWarningMessages", true);
user_pref("devtools.webconsole.input.autocomplete", true);
user_pref("devtools.webconsole.input.context", true);
user_pref("devtools.webconsole.input.eagerEvaluation", false);
user_pref("devtools.webconsole.input.editor", false);
user_pref("devtools.webconsole.persistlog", false);
user_pref("devtools.webconsole.timestampMessages", false);
