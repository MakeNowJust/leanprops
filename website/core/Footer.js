/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

class Footer extends React.Component {
  docUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    const docsUrl = this.props.config.docsUrl;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    return `${baseUrl}${docsPart}${langPart}${doc}`;
  }

  pageUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + (language ? `${language}/` : '') + doc;
  }

  render() {
    return (
      <footer className="nav-footer" id="footer">
        <section className="sitemap">
          <div>
            <h5>Docs</h5>
            <a href={this.docUrl('overview', this.props.language)}>
              Overview
            </a>
            <a href={this.docUrl('example', this.props.language)}>
              One-Minute Example
            </a>
          </div>
          <div>
            <h5>Modules</h5>
            <a href={this.docUrl('leanprops-core', this.props.language)}>
              leanprops-core
            </a>
            <a href={this.docUrl('leanprops-magnolia', this.props.language)}>
              leanprops-magnolia
            </a>
          </div>
          <div>
            <h5>Social</h5>
            <a
              href="https://twitter.com/make_now_just"
              target="_blank"
              rel="noreferrer noopener">
              Twitter
            </a>
            <a href="https://github.com/MakeNowJust/leanprops">GitHub</a>
          </div>
        </section>

        <section className="copyright">{this.props.config.copyright}</section>
      </footer>
    );
  }
}

module.exports = Footer;
