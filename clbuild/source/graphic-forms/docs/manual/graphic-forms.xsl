<?xml version="1.0"?>
<!--
    graphic-forms.xsl

    Copyright (c) 2006-2007, Jack D. Unrue
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:import href="htmlhelp.xsl"/>
  <xsl:param name="version"                            />
  <xsl:param name="chapter.autolabel"                  select="0"/>
  <xsl:param name="chunk.first.sections"               select="1"/>
  <xsl:param name="chunk.section.depth"                select="2"/>
  <xsl:param name="chunker.output.indent"              select="'yes'"/>
  <xsl:param name="generate.legalnotice.link"          select="0"/>
  <xsl:param name="generate.toc"                       />
  <xsl:param name="html.stylesheet"                    select="'graphic-forms.css'"/>
  <xsl:param name="htmlhelp.button.hideshow"           select="0"/>
  <xsl:param name="htmlhelp.button.forward"            select="1"/>
  <xsl:param name="htmlhelp.button.next"               select="0"/>
  <xsl:param name="htmlhelp.button.prev"               select="0"/>
  <xsl:param name="htmlhelp.chm"                       select="concat('graphic-forms-',$version,'.chm')"/>
  <xsl:param name="htmlhelp.hhc.folders.instead.books" select="0"/>
  <xsl:param name="htmlhelp.show.advanced.search"      select="1"/>
  <xsl:param name="suppress.navigation"                select="1"/>
<!--
     xsltproc doesn't implement the adjustColumnWidths function

  <xsl:param name="use.extensions"                     select="1"/>
  <xsl:param name="tablecolumn.extensions"             select="1"/>
-->
  <xsl:template name="user.footer.content">
    <div class="footer">
      Copyright &#x00A9; 2006-2007, Jack D. Unrue
    </div>
  </xsl:template>
  
</xsl:stylesheet>
