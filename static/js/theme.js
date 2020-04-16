function toggleTheme() {
    const dark = document.getElementById("dark");
    const light = document.getElementById("light");
    const button = document.getElementById("theme-button");
    
    if(dark !== undefined && dark.disabled !== undefined && dark.disabled === true) {
      dark.removeAttribute("disabled");
      light.setAttribute("disabled", true);
      button.innerText = "Dark";
    }
    else
    {
      light.removeAttribute("disabled");
      dark.setAttribute("disabled", true);
      button.innerText = "Light";
    }
  }