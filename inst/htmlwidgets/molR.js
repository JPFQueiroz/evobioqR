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

          // Load structure
          if (x.file) {
            molstarLib.loadStructure(viewer, x.file, { format: x.file.startsWith('http') ? null : "pdb" })
              .then(() => {
                // Apply selections using Molscript
                if (x.selections && Array.isArray(x.selections)) {
                  x.selections.forEach(async (selection) => {
                    try {
                      // Here we assume 'selection' is a string in the format 'A 12-200'
                      const query = molstarLib.molR_selection(selection, 'auth');
                      await plugin.managers.structure.selection.fromCompiledQuery('add', query);
                    } catch (err) {
                      console.error("Failed to apply Molscript selection:", selection, err);
                    }
                  });
                }

                // INSERT CUSTOM REPRESENTATION HERE
//                const customParams = {
//                  type: x.representationType || 'ball-and-stick', // default to ball-and-stick if not specified
//                  colorTheme: x.colorTheme || 'uniform', // default to uniform if not specified
//                  sizeTheme: x.sizeTheme || 'uniform', // default to uniform if not specified
//                  ignoreHydrogens: x.ignoreHydrogens || false,
//                  quality: x.quality || 'auto',
//                  carbonColor: x.carbonColor || 'chain-id',
//                  showCarbohydrateSymbol: x.showCarbohydrateSymbol || false
//                };
//                molstarLib.applyCustomRepresentation(plugin, customParams);
//
//                // Apply other visual effects
//                if (x.transparency > 0) {
//                  molstarLib.setTransparency(viewer, x.transparency);
//                }
//                if (x.overpaintColor) {
//                  molstarLib.setOverpaintColor(plugin, x.overpaintColor);
//                }
//                if (x.emissiveStrength > 0) {
//                  molstarLib.setEmissiveStrength(plugin, x.emissiveStrength);
//                }
//                if (x.substanceMaterial) {
//                  molstarLib.setSubstanceMaterial(plugin, x.substanceMaterial);
//                }

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
