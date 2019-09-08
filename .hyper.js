// HyperTerm, installed as a joke: https://hyper.is#cfg
module.exports = {
	keymaps:      {},
	plugins:      [],
	localPlugins: [], // Devlinks: ~/.hyper_plugins/local/
	config: {
		updateChannel:     "stable",
		fontSize:          14,
		fontFamily:        'Menlig, Menlo, Menloco, "DejaVu Sans Mono", Consolas, "Lucida Console", monospace',
		fontWeight:        "normal",
		fontWeightBold:    "bold",
		lineHeight:        1,
		letterSpacing:     0,
		cursorColor:       "#606060",
		cursorAccentColor: "#000",
		cursorShape:       "BEAM",
		cursorBlink:       false,
		foregroundColor:   "#fff",
		backgroundColor:   "rgba(0,0,0,0.85)",
		selectionColor:    "rgba(82,82,82)",
		borderColor:       "#333",
		padding:           "12px 14px",
		css:               "", // Custom CSS (main window)
		termCSS:           "", // custom CSS (terminal window)
		env:               {}, // Environment variables
		bell:              false,
		defaultSSHApp:     false,
		webGLRenderer:     true,
	},
};
