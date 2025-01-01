HTMLWidgets.widget({
  name: "MolstarVieweR",
  type: "output",
  factory: function (el, width, height) {
    let viewer;

    return {
      renderValue: function (x) {
        el.innerHTML = ""; // Clear previous content

        // Ensure Molstar library is loaded
        if (typeof molstar === "undefined") {
          console.error("Molstar library is not loaded");
          el.innerHTML = "Error: Molstar library failed to load.";
          return;
        }

        // Initialize the viewer
        molstar.Viewer.create(el.id, {
          layoutShowControls: false,
          collapseLeftPanel: true,
          viewportShowExpand: false,
          pdbProvider: 'pdbe',
          emdbProvider: 'pdbe',
          volumeStreamingServer: 'https://www.ebi.ac.uk/pdbe/densities',
          pixelScale: 1,
          pickScale: 0.25,
          pickPadding: 1,
          preferWebgl1: false,
          powerPreference: 'high-performance',
        }).then(v => {
          viewer = v;

          // Load structure from PDB URL or data
          if (x.pdbUrl) {
            viewer.loadStructureFromUrl(x.pdbUrl, "pdb");
          } else if (x.pdbData) {
            viewer.loadStructureFromData(x.pdbData, "pdb");
          } else if (x.pdbid) {
            viewer.loadPdb(x.pdbid, "pdb");
          }        });
      },
      resize: function (width, height) {
        if (viewer) viewer.handleResize();
      },
    };
  },
});
