const siteConfig = {
  title: "LeanProps",
  tagline: "Simple enumerative property-based testing for Scala",
  url: "https://makenowjust.github.com",
  baseUrl: "/leanprops/",

  projectName: "leanprops",
  organizationName: "MakeNowJust",

  headerLinks: [
    { doc: "overview", label: "Docs" },
    { href: "https://github.com/MakeNowJust/leanprops", label: "GitHub" }
  ],

  colors: {
    primaryColor: "#8E2555",
    secondaryColor: "#5C203B"
  },

  customDocsPath: "website/target/mdoc",

  copyright: `(C) ${new Date().getFullYear()} TSUYUSATO "MakeNowJust" Kitsune`,

  highlight: {
    theme: "default"
  },

  scripts: [],
  onPageNav: "separate",
  cleanUrl: true,

  ogImage: "img/docusaurus.png",
  twitterImage: "img/docusaurus.png"
};

module.exports = siteConfig;
