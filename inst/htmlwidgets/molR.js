HTMLWidgets.widget({
  name: "molR",
  type: "output",
  factory: function (el, width, height) {
    let viewer;

    return {
      renderValue: function (x) {
        el.innerHTML = ""; // Clear previous content

        // Ensure Molstar library is loaded via molstarLib
        if (typeof molstarLib === "undefined") {
          console.error("Molstar library is not loaded");
          el.innerHTML = "Error: Molstar library failed to load.";
          return;
        }

        // Initialize the Molstar viewer using the molstarLib
        molstarLib.initViewer(el.id).then(plugin => {
          viewer = plugin;

          // Load structure or MVSJ state based on fileType
          if (x.file) {
            if (x.fileType === "pdb") {
              // Existing PDB loading logic
              molstarLib.loadStructure(viewer, x.file, {
                format: x.file.startsWith('http') ? null : "pdb",
                overrideRepresentation: x.overrideRepresentation,
                addRepresentation: x.addRepresentation,
                quickStyles: x.quickStyles
              })
                .then(preset => {
                  // Apply selectionStyles
                  if (x.selectionStyles && Array.isArray(x.selectionStyles)) {
                    x.selectionStyles.forEach(async (style) => {
                      try {
                        await molstarLib.applySelectionStyle(
                          viewer,
                          preset,
                          style.selection,
                          style.representation || 'molecular-surface',
                          style.color || 'uniform'
                        );
                      } catch (err) {
                        console.error("Failed to apply selection style for:", style.selection, err);
                      }
                    });
                  }

                  // Change background color
                  if (x.backgroundColor) {
                    const renderer = plugin.canvas3d && plugin.canvas3d.props.renderer;
                    molstarLib.setBackgroundColor(plugin, renderer, x.backgroundColor);
                  }
                })
                .catch((err) => {
                  console.error("Failed to load structure from data:", err);
                  el.innerHTML = "Failed to load structure from data.";
                });
            } else if (x.fileType === "mvsj") {
              // Load MVSJ state
              molstarLib.loadMVSJState(viewer, x.file, {
                sourceUrl: undefined, // No source URL for local files
                sanityChecks: true, // Enable validation
                replaceExisting: true // Clear existing state
              })
                .then(() => {
                  // Apply background color if specified (optional override)
                  if (x.backgroundColor) {
                    const renderer = plugin.canvas3d && plugin.canvas3d.props.renderer;
                    molstarLib.setBackgroundColor(plugin, renderer, x.backgroundColor);
                  }
                })
                .catch((err) => {
                  console.error("Failed to load MVSJ state:", err);
                  el.innerHTML = "Failed to load MolViewSpec state: " + err.message;
                });
            } else {
              console.warn("Unsupported file type:", x.fileType);
              el.innerHTML = "Unsupported file type.";
            }
          } else {
            console.warn("No structure data provided.");
            el.innerHTML = "No structure data provided.";
          }
        }).catch(err => {
          console.error("Failed to initialize viewer:", err);
          el.innerHTML = "Failed to initialize Molstar viewer.";
        });
      },
      resize: function (width, height) {
        if (viewer) viewer.handleResize();
      },
    };
  },
});
