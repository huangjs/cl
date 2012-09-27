<?xml version="1.0"?>
<!--
    packages.xsl

    Copyright (c) 2007, Jack D. Unrue
-->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  version="1.0">

  <xsl:output method="xml" omit-xml-declaration="yes" indent="yes"/>

  <xsl:include href="utils.xsl"/>

  <xsl:param name="nickname"/>

  <xsl:template match="/packages/package">

    <xsl:if test="@name=$nickname">

    <chapter>
      <title><xsl:value-of select="@fullname"/></title>
      <indexterm>
        <primary>
          <xsl:call-template name="upcase">
            <xsl:with-param name="orig-text"><xsl:value-of select="@name"/></xsl:with-param>
          </xsl:call-template>
        </primary>
      </indexterm>
      <indexterm><primary><xsl:value-of select="@fullname"/></primary></indexterm>

      <para role="normal">[Package]</para>

      <bridgehead renderas="sect2">nickname</bridgehead>
      <para role="normal">
        <xsl:call-template name="upcase">
          <xsl:with-param name="orig-text"><xsl:value-of select="@name"/></xsl:with-param>
        </xsl:call-template>
      </para>

      <xsl:for-each select="description">
        <xsl:element name="bridgehead">
          <xsl:attribute name="renderas">sect2</xsl:attribute>
          description
        </xsl:element>

        <xsl:element name="para">
          <xsl:attribute name="role">normal</xsl:attribute>
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:for-each>

      <xsl:for-each select="symbol-files/file">
        <xsl:variable name="filename" select="concat(@basename,'.xml')"/>

        <xsl:if test="document($filename)/symbols/class | document($filename)/symbols/structure">
          <bridgehead renderas="sect2">classes and structures</bridgehead>
          <para role="normal">
            <xsl:for-each select="document($filename)/symbols/class | document($filename)/symbols/structure">
              <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
              <xsl:element name="link">
                <xsl:attribute name="linkend"><xsl:value-of select="$nickname"/>:<xsl:value-of select="@name"/></xsl:attribute>
                <xsl:value-of select="@name"/>
              </xsl:element>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </para>
        </xsl:if>

        <xsl:if test="document($filename)/symbols/generic-function">
          <bridgehead renderas="sect2">generic functions</bridgehead>
          <para role="normal">
            <xsl:for-each select="document($filename)/symbols/generic-function">
              <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
              <xsl:element name="link">
                <xsl:attribute name="linkend"><xsl:value-of select="$nickname"/>:<xsl:value-of select="@name"/></xsl:attribute>
                <xsl:value-of select="@name"/>
              </xsl:element>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </para>
        </xsl:if>

        <xsl:if test="document($filename)/symbols/function | document($filename)/symbols/slot-accessor | document($filename)/symbols/slot-reader">
          <bridgehead renderas="sect2">accessors and simple functions</bridgehead>
          <para role="normal">
            <xsl:for-each select="document($filename)/symbols/function | document($filename)/symbols/slot-accessor | document($filename)/symbols/slot-reader">
              <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
              <xsl:element name="link">
                <xsl:attribute name="linkend"><xsl:value-of select="$nickname"/>:<xsl:value-of select="@name"/></xsl:attribute>
                <xsl:value-of select="@name"/>
              </xsl:element>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </para>
        </xsl:if>

        <xsl:if test="document($filename)/symbols/macro">
          <bridgehead renderas="sect2">macros</bridgehead>
          <para role="normal">
            <xsl:for-each select="document($filename)/symbols/macro">
              <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
              <xsl:element name="link">
                <xsl:attribute name="linkend"><xsl:value-of select="$nickname"/>:<xsl:value-of select="@name"/></xsl:attribute>
                <xsl:value-of select="@name"/>
              </xsl:element>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </para>
        </xsl:if>

        <xsl:if test="document($filename)/symbols/condition">
          <bridgehead renderas="sect2">conditions</bridgehead>
          <para role="normal">
            <xsl:for-each select="document($filename)/symbols/condition">
              <xsl:sort select="@name" order="ascending" case-order="upper-first"/>
              <xsl:element name="link">
                <xsl:attribute name="linkend"><xsl:value-of select="$nickname"/>:<xsl:value-of select="@name"/></xsl:attribute>
                <xsl:value-of select="@name"/>
              </xsl:element>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </para>
        </xsl:if>

      </xsl:for-each>

      <bridgehead renderas="sect2">
        see also

        <para role="normal">
          <link linkend="constants">Constants and Variables</link>
        </para>
      </bridgehead>

      <xsl:variable name="sections">
        <xsl:for-each select="symbol-files/file">
          <xsl:variable name="filename" select="concat(@basename,'-tmp.xml')"/>
          <xsl:for-each select="document($filename)/data/section">
            <xsl:copy-of select="current()"/>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="exsl:node-set($sections)/section">
        <xsl:sort select="current()/indexterm/primary" order="ascending" case-order="upper-first"/>
        <xsl:copy-of select="current()"/>
      </xsl:for-each>

    </chapter>

    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
